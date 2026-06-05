% ============================================================================
% CONSTRAINT STORY: sotu_1946_truman_war_liquidation_peacetime_reconversion_program
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1946_truman_war_liquidation_peacetime_reconversion_program, []).

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
 *   constraint_id: sotu_1946_truman_war_liquidation_peacetime_reconversion_program
 *   human_readable: Truman War Liquidation and Peacetime Reconversion Program (1946)
 *   domain: military/economic_policy/institutional_coordination
 *
 * SUMMARY:
 *   The Truman administration's 1946 war liquidation and peacetime
 *   reconversion program represents a massive institutional challenge:
 *   systematically scaling down a total-war military-industrial complex while
 *   redirecting resources to civilian economy and domestic consumption. The
 *   program coordinated demobilization of 12 million military personnel,
 *   closure or retooling of thousands of defense facilities, reallocation of
 *   scarce materials and manufacturing capacity, and retraining of millions
 *   of workers. Simultaneously, the program was constrained by security
 *   considerations: the Soviet Union remained militarily formidable, and the
 *   U.S. maintained occupation forces in Europe, Asia, and the Pacific. The
 *   constraint exhibits the classic structure of a tangled_rope: genuine
 *   coordination function (managing a complex transition without economic
 *   collapse) coexists with substantial asymmetric extraction (massive
 *   employment losses, concentrated among workers and regional defense
 *   contractor communities, while benefits accrue to peacetime consumer
 *   industries and reconstruction sectors). The theater_ratio (0.38) is
 *   relatively low because the reconversion program had explicit, measurable
 *   objectives and limited performative overhead — though some planning
 *   apparatus degradation is observable as wartime coordination mechanisms
 *   lose relevance in peacetime markets. The extractiveness trajectory (0.68
 *   → 0.45 over 6 years) shows declining extraction as the immediate post-war
 *   dislocation period gives way to stabilized peacetime economy, consistent
 *   with scaffold's sunset logic.
 *
 * KEY AGENTS:
 *   - War Production Workers and Military Personnel: Primary victims (powerless/trapped) — face immediate unemployment and dislocation with limited retraining support; no exit options from demobilization
 *   - Regional Defense Contractor Communities: Secondary victims (moderate/constrained) — lose defense contracts and must retool for civilian production; constrained by capital conversion costs but benefit from reopened consumer markets
 *   - Peacetime Economy and Consumer Sector: Primary beneficiaries (institutional/arbitrage) — gain access to manufacturing capacity, materials, and labor; experience the reconversion as pure coordination enabling market expansion
 *   - Labor Organizations and Reconversion Planners: Organized coordinators (organized/constrained) — possess moderate leverage to negotiate worker protections and retraining support; perceive the mechanism as temporary transition with declared endpoint
 *   - Military-Industrial Planning Establishment: Institutional maintainer (institutional/arbitrage) — experiences degradation as wartime coordination apparatus becomes less functionally necessary; maintains theater through committee structures and planning protocols
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the contingent institutional choices of U.S. reconversion as inevitable features of military-state transitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, 0.52).
domain_priors:suppression_score(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, 0.48).
domain_priors:theater_ratio(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, tangled_rope).
narrative_ontology:human_readable(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, "Truman War Liquidation and Peacetime Reconversion Program (1946)").
narrative_ontology:topic_domain(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, "military/economic_policy/institutional_coordination").

domain_priors:requires_active_enforcement(sotu_1946_truman_war_liquidation_peacetime_reconversion_program).
narrative_ontology:has_sunset_clause(sotu_1946_truman_war_liquidation_peacetime_reconversion_program).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, peacetime_economy).
narrative_ontology:constraint_beneficiary(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, domestic_programs).
narrative_ontology:constraint_beneficiary(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, civilian_consumer_sector).
narrative_ontology:constraint_victim(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, military_industrial_employment).
narrative_ontology:constraint_victim(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, defense_contractors).
narrative_ontology:constraint_victim(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, war_production_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAR PRODUCTION WORKERS AND MILITARY PERSONNEL (SNARE) — Trapped by demobilization requirements and factory closures. Millions facing immediate unemployment with minimal retraining support. No meaningful exit options from the reconversion mechanism; cannot opt out of demobilization or plant closures. Maximum experienced extraction: loss of wartime employment, disrupted career trajectories, housing instability as defense contracts terminate. Theater is minimal — the extraction mechanism is direct and structural, not performative.
constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL DEFENSE CONTRACTOR COMMUNITIES (TANGLED ROPE) — Constrained exit through economic dependency on defense contracts but also benefit from peacetime infrastructure investment and consumer economy reopening. Significant extraction (loss of contracts, capacity underutilization) alongside genuine coordination function (reconversion to civilian production, supply chain restructuring). Costs are localized and immediate; benefits are distributed over longer horizon. Moderate organizational capacity to advocate but limited leverage with federal reconversion planners.
constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PEACETIME ECONOMY AND CONSUMER SECTOR (ROPE) — Primary beneficiary with high arbitrage capacity. Gains access to scarce materials, manufacturing capacity, and labor now freed from war production. The constraint is experienced as pure coordination: managing the reallocation of resources from military to civilian use. Benefits are immediate and substantial. Institutional actors (consumer goods manufacturers, retailers, infrastructure developers) can articulate their interests and influence reconversion priorities. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR ORGANIZATIONS AND RECONVERSION PLANNERS (SCAFFOLD) — Organized agents see the reconversion mechanism as temporary, with explicit sunset logic built into the program structure. Reconversion is designed to be a transition process (1946-1950) with planned endpoint, not a permanent extraction mechanism. Theater is relatively low because the program has measurable objectives: retraining workers, redirecting facilities, establishing peacetime employment baselines. Labor has moderate leverage through collective action and political negotiation. Scaffold classification derives from the declared sunset: as peacetime economy stabilizes and retraining completes, the reconversion constraints are scheduled to dissolve.
constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MILITARY-INDUSTRIAL PLANNING ESTABLISHMENT (PITON) — The planning apparatus (War Production Board, Office of War Mobilization and Reconversion, military logistics organs) experiences degradation. Wartime coordination mechanisms optimized for total mobilization have limited functional utility in peacetime market economy. The establishment maintains its theater (planning committees, reconversion guidelines, coordination protocols) through institutional inertia despite reduced operational necessity. Many planning functions are performative — the actual resource reallocation happens through market mechanisms and direct producer-consumer negotiation, not through formal reconversion directives. Theater ratio reflects the gap between the planning apparatus's projected role and its actual function in a partially-reconverted economy.
constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INSTITUTIONAL LOGIC VIEW (MOUNTAIN) — From a civilizational/universal perspective, the transition from total war mobilization to peacetime capitalism appears as an immutable structural law: all major wars end in demobilization, and all demobilizations involve economic dislocation. The reconversion mechanism might appear as inherent to the logic of military-state transitions — no nation escapes the need to demobilize. However, the structural data contradicts this naturalization: the U.S. reconversion program in 1946 is contingent on specific institutional choices (GI Bill funding, defense procurement policy, labor protections, industrial policy). Neighboring nations chose different reconversion paths (Soviet militarization, Western European worker protections). The engine will flag this as a false summit revealing naturalization of political choices.
constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1946_truman_war_liquidation_peacetime_reconversion_program_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, TR),
    TR >= 0.70.

:- end_tests(sotu_1946_truman_war_liquidation_peacetime_reconversion_program_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high but declining over the interval. The initial extractiveness (0.68) reflects the massive immediate dislocation: millions losing defense employment simultaneously, with insufficient retraining capacity and weak labor market absorption. The declining trajectory (0.68 → 0.45 over 6 years) reflects successful structural absorption as reconverted capacity enables peacetime production, real wages recover, and unemployment rates decline toward natural levels. The measurement trajectory is consistent with the declared sunset clause: extraction is high during the active transition period but scheduled to decline as the economy stabilizes. Suppression (0.48): Moderate. Barriers to exit from the reconversion mechanism include: mandatory demobilization (legal obligation for military personnel), plant closure decisions by federal/corporate ownership (limited worker agency), geographic immobility due to defense plant concentration, and significant skill mismatch between military production and civilian manufacturing. However, suppression is not total — labor markets eventually absorb workers, some defense contractors successfully transition, and the program itself includes worker protections and retraining support (reducing suppression). Theater ratio (0.38): Low-moderate, increasing slightly over time. The reconversion program has explicit, measurable objectives and operates through direct institutional mechanisms (facility closure dates, contract termination schedules, retraining funding). However, some theater is present: the planning establishment's coordination role is partly performative (actual reallocation occurs through market mechanisms); retraining programs' effectiveness is sometimes theatrical (programs exist but job placement outcomes are mixed); the 'minimum necessary security capacity' framing masks continued defense procurement and Cold War preparation.
 *
 * PERSPECTIVAL GAP:
 *   The reconversion program's classification varies sharply across perspectives despite identical base properties. War production workers see a snare because they experience maximum extraction (job loss, dislocation) with no meaningful exit — the mechanism appears coercive and inescapable. Regional contractor communities see a tangled rope because they lose contracts (extraction) but can retool for civilian production (exit option) and eventually benefit from consumer market growth (genuine coordination). Peacetime consumer sector sees pure coordination (rope) because they experience the constraint as simply reallocating resources to their productive use — no extraction is perceptible to them. Organized labor and reconversion planners see a temporary mechanism with sunset clause (scaffold) because the program is explicitly designed as a 4-6 year transition, with institutional endpoints built into the structure. The military-industrial planning establishment sees a degraded ritual (piton) because the coordination apparatus that was vital during wartime has become partly performative in peacetime markets. The analytical observer risks seeing an immutable law (mountain) — the inevitable dislocation of demobilization — but structural analysis reveals this as naturalization of contingent institutional choices (false summit). The gap between these perspectives reflects genuine structural differences in power, exit options, and benefit flows, not mere disagreement about framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to the extraction flow. War production workers: d ≈ 0.95 (full victims, trapped with no exit, direct loss of employment and income). Defense contractor communities: d ≈ 0.75 (significant victims due to contract loss and capacity conversion costs, but constrained exit allows some transition and eventual benefit from consumer market opening). Peacetime consumer sector: d ≈ 0.10 (full beneficiaries with arbitrage exit options, gaining access to production capacity and materials). Labor organizations: d ≈ 0.50 (symmetric position: bear retraining costs and employment disruption, benefit from wage recovery and peacetime employment; organized exit options allow some mitigation). Military-industrial planning: d ≈ 0.25 (institutional beneficiary through maintenance of defense capacity and planning apparatus, despite degraded functional role). Analytical observer: d ≈ 0.72 (observational position, neither beneficiary nor victim, but bearing epistemic cost of false naturalization).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating how a single institutional mechanism can be simultaneously: (1) pure extraction for powerless trapped agents (snare), (2) mixed coordination-extraction for constrained moderate agents (tangled rope), (3) pure coordination for institutional beneficiaries (rope), (4) temporary transition for organized actors with sunset (scaffold), (5) degraded theater for institutional maintainers (piton), and (6) naturalizable as law for civilizational observers (mountain). The mandatrophy is not resolved by choosing a single 'true' type but by recognizing that the classification system generates six simultaneous, structurally justified readings from a single set of base properties. The claimed_type (tangled_rope) represents the dominant structure: the mechanism genuinely coordinates a complex transition AND extracts asymmetrically from workers to benefit peacetime industries. This is neither pure coordination nor pure extraction — it is the hybrid form that makes tangled_rope the accurate classification. The perspectival pluralism is not a problem to solve but the actual structure to model: the constraint exists as all six types simultaneously, indexed to different positions in the institutional network.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconversion_pace_asymmetry,
    'Does the reconversion mechanism extract more from workers than from contractors through timing asymmetries in employment loss vs. retooling opportunities?',
    'Comparative analysis of unemployment duration, wage recovery trajectories, and capital loss for workers vs. contractors. Measurement of time lag between production cuts and redeployment to civilian markets.',
    'If extraction is asymmetric: the snare and tangled_rope classifications are reinforced; reconversion is not pure coordination. If symmetric: the rope perspective gains weight, and the constraint approaches pure coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconversion_pace_asymmetry, empirical, 'Timing asymmetry in employment loss vs. contractor retooling').

omega_variable(
    defense_capacity_retention_hidden,
    'Is the reconversion program''s official goal (minimum necessary security capacity) masking retention of wartime industrial capacity for Cold War preparation?',
    'Comparison of capacity retention rhetoric vs. actual maintenance of defense plants, machine tools, and logistics. Historical analysis of defense budget trends 1946-1950 and declassified strategic planning documents.',
    'If retention is genuine peacetime need: tangled_rope classification holds. If retention is covert war preparation: the snare and piton classifications strengthen (theater masking militarization; extraction sustaining weapons industries).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_capacity_retention_hidden, empirical, 'Whether ''minimum necessary'' security capacity masks Cold War preparation').

omega_variable(
    retraining_sufficiency_vs_theater,
    'Do federal retraining programs (vocational education, relocation assistance) provide genuine occupational mobility or function primarily as theater legitimizing job losses?',
    'Longitudinal employment data: fraction of retraining participants achieving equivalent-or-better wage employment within 2 years. Comparison of wage trajectories with/without federal retraining support.',
    'If retraining is effective: scaffold classification reinforced; theater_ratio overestimated. If largely ineffective: piton classification strengthens; theater_ratio reflects reality of performative programs masking dislocation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retraining_sufficiency_vs_theater, empirical, 'Whether retraining programs provide genuine mobility or merely legitimize losses').

omega_variable(
    peacetime_beneficiary_concentration,
    'Are the benefits of reconversion distributed broadly to the civilian economy or concentrated among large consumer goods manufacturers and financial interests?',
    'Input-output analysis of production reallocation flows. Measurement of capital distribution across firm sizes and sectors. Analysis of government contracts vs. private market transactions in reconverted capacity.',
    'If benefits widely distributed: rope classification reinforced. If concentrated: tangled_rope strengthens (hidden extraction mechanism benefiting concentrated economic interests at dispersed worker cost).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peacetime_beneficiary_concentration, empirical, 'Concentration vs. distribution of reconversion benefits').

omega_variable(
    sunset_clause_enforcement,
    'Is the declared sunset (transition to peacetime economy by ~1950) actually enforced, or does reconversion persist indefinitely through Cold War remilitarization?',
    'Timeline analysis: when do reconversion agencies dissolve? When does defense spending stop declining? Correlation with Cold War escalation (NATO formation, Korean War onset).',
    'If sunset enforced: scaffold classification holds. If sunset indefinitely deferred: scaffold degrades to snare or tangled_rope; the temporary mechanism becomes permanent extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sunset_clause_enforcement, empirical, 'Whether stated sunset clause is actually enforced or indefinitely deferred').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_1946_tr_t0, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, theater_ratio, 0, 0.32).
narrative_ontology:measurement(sotu_1946_tr_t2, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, theater_ratio, 2, 0.35).
narrative_ontology:measurement(sotu_1946_tr_t4, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, theater_ratio, 4, 0.38).
narrative_ontology:measurement(sotu_1946_tr_t6, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, theater_ratio, 6, 0.42).

% Extraction over time
narrative_ontology:measurement(sotu_1946_be_t0, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(sotu_1946_be_t2, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, base_extractiveness, 2, 0.61).
narrative_ontology:measurement(sotu_1946_be_t4, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(sotu_1946_be_t6, sotu_1946_truman_war_liquidation_peacetime_reconversion_program, base_extractiveness, 6, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, resource_allocation).
narrative_ontology:affects_constraint(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, cold_war_militarization_escalation).
narrative_ontology:affects_constraint(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, postwar_labor_market_absorption).
narrative_ontology:affects_constraint(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, defense_industrial_capacity_retention).

% DUAL FORMULATION NOTE:
% The war liquidation program is a single structural constraint but operates through multiple decomposable dimensions: (1) military demobilization (constrains military personnel and defense establishments), (2) production facility conversion (constrains contractors and regional economies), (3) worker retraining and labor market absorption (constrains workers and consumer sector employment). Each dimension has different ε values and extraction mechanisms. The network relationships identify downstream constraints affected by reconversion: Cold War remilitarization occurs partially because reconversion was incomplete; labor absorption depends on peacetime economic growth enabled by resource reallocation; defense capacity retention depends on security policy choices during reconversion.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1946_truman_war_liquidation_peacetime_reconversion_program, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
