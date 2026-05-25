% ============================================================================
% CONSTRAINT STORY: ukraine_labor_market_mobilization_drag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukraine_labor_market_mobilization_drag, []).

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
 *   constraint_id: ukraine_labor_market_mobilization_drag
 *   human_readable: Ukraine Labor Market Mobilization Drag
 *   domain: economic_policy/labor_market/wartime_coordination
 *
 * SUMMARY:
 *   Ukraine's labor mobilization system during the 2022-present conflict
 *   creates structural tension between legitimate wartime resource
 *   coordination and institutional extraction through conscription
 *   enforcement. The constraint exhibits hybrid character: genuine
 *   coordination function (securing workforce for existential defense)
 *   coexists with asymmetric extraction (military command captures allocation
 *   decisions, defense firms benefit from labor coercion, civilians bear
 *   suppression and wage loss). The system combines Soviet-era administrative
 *   apparatus (TsSU registry, conscription boards) with wartime emergency
 *   authorization, creating high suppression (0.72) through legal penalties,
 *   informal coercion, and constrained exit options. Theater ratio (0.65)
 *   reflects that mobilization enforcement relies on selective prosecution
 *   and corruption rather than systematic capacity — the apparatus performs
 *   mobilization bureaucratically without fully integrating modern labor
 *   market information. Extractiveness (0.58) increased from 0.35 at the
 *   start of mobilization as conscription expanded beyond initial volunteer
 *   phase, initial military needs were met, and institutional drag
 *   accumulated in wage suppression and small-business labor constraints.
 *
 * KEY AGENTS:
 *   - Conscripted Workers: Primary victims (powerless/trapped) — face legal mobilization orders, loss of wages, family separation, mortality risk, no legal exit mechanism except death or disability
 *   - Civilian Workforce (Remaining): Secondary victims (moderate/constrained) — reduced labor supply raises prices, constrains business operations, faces informal mobilization pressure for critical sectors
 *   - Small/Medium Enterprises: Secondary victims (moderate/constrained) — lose key employees to conscription, face hiring restrictions, price controls on critical inputs; some benefit from state contracts if deemed critical
 *   - Military Command Structure: Primary beneficiary (institutional/arbitrage) — controls labor allocation, receives priority workforce, can exit through reassignment; benefits from mobilization as coordination mechanism for securing defense workforce
 *   - Defense Industry Firms: Primary beneficiary (institutional/arbitrage) — receive priority labor allocation, guaranteed contracts, state subsidies for production; view mobilization as solving labor scarcity coordination problem
 *   - State Budget Allocation: Beneficiary (institutional/arbitrage) — avoids civilian wage compression through conscription; mobilization suppresses wage inflation that would force higher social spending
 *   - International Economic Institutions: Organized agents (organized/constrained) — IMF/World Bank/EU see mobilization as temporary with post-war sunset; have leverage to enforce demobilization timelines as condition of reconstruction aid
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing wartime mobilization as immutable law of conflict necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukraine_labor_market_mobilization_drag, 0.58).
domain_priors:suppression_score(ukraine_labor_market_mobilization_drag, 0.72).
domain_priors:theater_ratio(ukraine_labor_market_mobilization_drag, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukraine_labor_market_mobilization_drag, extractiveness, 0.58).
narrative_ontology:constraint_metric(ukraine_labor_market_mobilization_drag, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ukraine_labor_market_mobilization_drag, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukraine_labor_market_mobilization_drag, tangled_rope).
narrative_ontology:human_readable(ukraine_labor_market_mobilization_drag, "Ukraine Labor Market Mobilization Drag").
narrative_ontology:topic_domain(ukraine_labor_market_mobilization_drag, "economic_policy/labor_market/wartime_coordination").

domain_priors:requires_active_enforcement(ukraine_labor_market_mobilization_drag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukraine_labor_market_mobilization_drag, military_command_structure).
narrative_ontology:constraint_beneficiary(ukraine_labor_market_mobilization_drag, defense_industry_firms).
narrative_ontology:constraint_beneficiary(ukraine_labor_market_mobilization_drag, state_budget_allocation).
narrative_ontology:constraint_victim(ukraine_labor_market_mobilization_drag, civilian_workforce).
narrative_ontology:constraint_victim(ukraine_labor_market_mobilization_drag, small_medium_enterprises).
narrative_ontology:constraint_victim(ukraine_labor_market_mobilization_drag, consumer_purchasing_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONSCRIPTED WORKER (SNARE) — Faces legal mobilization orders with criminal penalties for non-compliance. No legal exit or deferment except through death or permanent disability. Bears full extraction cost: lost wages, family separation, mortality risk. Cannot organize collective action without military prosecution.
constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Faces de facto labor mobilization pressure: key employees conscripted, hiring restrictions, price controls on critical inputs. Benefits from state contracts and priority supply access if enterprise is deemed critical; constrained by informal quotas and production mandates. High exit costs (asset seizure, license revocation) but not impossible. Mixed extraction and coordination function.
constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEFENSE INDUSTRY FIRM (ROPE) — Primary beneficiary. Receives priority labor allocation, guaranteed contracts, state subsidies. Views mobilization as coordination mechanism for securing skilled workforce. Can exit through relocation or contract renegotiation. Net extraction flows toward this agent.
constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTL ECONOMIC COOPERATION (SCAFFOLD) — IMF/World Bank/EU frameworks condition aid on labor market reforms and demobilization timelines post-war. See mobilization drag as temporary emergency measure with built-in sunset: EU accession requirements, NATO standards, post-reconstruction employment mandates all presuppose return to market labor allocation. Organized agents (international institutions) have leverage to enforce sunset.
constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: SOVIET-ERA APPARATUS (PITON) — State mobilization structures (TsSU registry, military conscription boards, collective farm remnants) persist from USSR administration. Mobilization rhetoric and procedures are largely theatrical — actual enforcement relies on informal networks, corruption, and selective prosecution rather than systematic capacity. Theater ratio high: the apparatus is maintained through inertia despite limited functional integration with modern labor market data systems.
constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / WARTIME NECESSITY (MOUNTAIN) — From a civilizational perspective, mobilization drag may appear as an immutable constraint of warfare itself: existential conflict requires subordinating civilian labor markets to military demand. The observer risks naturalizing a contingent institutional arrangement (post-Soviet state capacity + Russian invasion) as a law of wartime necessity.
constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukraine_labor_market_mobilization_drag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukraine_labor_market_mobilization_drag, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukraine_labor_market_mobilization_drag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ukraine_labor_market_mobilization_drag, TR),
    TR >= 0.70.

:- end_tests(ukraine_labor_market_mobilization_drag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Initial phase (0-6 months) extractiveness was lower (0.35) because mobilization addressed genuine labor shortage and most participants were volunteers accepting coordination necessity. As conscription expanded beyond initial phase, benefits concentrated: defense firms secured labor supply at suppressed wages, state budget avoided wage inflation, military command secured personnel. Extractiveness increased to 0.48 at 6 months and 0.58 by 12 months as extraction mechanisms became routine rather than emergency-phase exception. Suppression (0.72): High and sustained. Criminal penalties for non-compliance with mobilization orders (up to 10 years imprisonment), seizure of assets, license revocation for businesses. Informal suppression through selective prosecution — enforcement is neither uniform nor transparent, creating chilling effect broader than formal penalties. Family separation creates psychological suppression. Wage controls for critical sectors add economic suppression. Theater ratio (0.65): Moderate-high and increasing. Mobilization apparatus performs bureaucratically but with limited actual capacity: TsSU registry relies on Soviet-era data systems, conscription boards operate on informal networks and corruption, enforcement is selective rather than systematic. Theater increases over time as the apparatus builds routine procedures that appear systematic but depend on informal enforcement and patronage networks.
 *
 * PERSPECTIVAL GAP:
 *   The gap between powerless conscript (maximum extraction, Snare) and defense firm (low extraction, Rope) reveals the constraint's hybrid character. Both perspectives are veridical: conscription genuinely extracts labor and imposes mortality risk on conscripts; mobilization genuinely solves coordination problem of securing defense workforce. The gap is not an observer error but a structural feature of the constraint's design. Conscripts bear costs; beneficiaries capture gains. The constraint cannot be classified from a single perspective — it requires the presheaf to capture both extraction (Snare pole) and coordination (Rope pole).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position: power level, exit options, and beneficiary/victim status. Conscripted workers have d ≈ 0.95 (trapped victims): maximum experienced extraction through f(d). Defense industry firms have d ≈ 0.10 (arbitrage beneficiaries): minimum extraction, net subsidy. Small business owners have d ≈ 0.65 (constrained secondary victims): moderate extraction through constrained exit + victim status. State budget allocation has d ≈ 0.05 (institutional beneficiary): extracted wages flow toward it, f(d) produces negative or minimal effective extraction. International institutions have d ≈ 0.55 (organized constrained): moderate experienced extraction because they have agency (leverage via conditionality) but must navigate domestic political constraints. Soviet apparatus has d ≈ 0.20 (institutional beneficiary with arbitrage exit through institutional continuation): low directionality favoring maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by distinguishing genuine coordination necessity from extractive institutional capture. The wartime defense production problem is real (coordination is necessary). The labor market clearing problem is real (workers need to be allocated to defense production). But the institution designed to solve it (conscription + suppression) distributes costs and benefits asymmetrically. The mandatrophy resolution shows: Snare from conscript perspective (pure extraction); Rope from military beneficiary perspective (pure coordination); Tangled Rope from analytical view that integrates both (genuine coordination necessity coexisting with asymmetric extraction). No single type is 'correct' — the classification presheaf over the observation site is the answer. The constraint is both genuinely necessary for wartime production AND genuinely extractive from conscript perspective. The mandatrophy is resolved by accepting both truths simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_mobilization_coercion_intensity,
    'Is the measured suppression (0.72) driven by legal enforcement capacity or by internalized social obligation and identity-fusion with collective defense?',
    'Comparative analysis of compliance rates in regions with high vs low corruption; tracking of voluntary vs coerced enrollment; post-war survey data on internalization of mobilization framing; exit behavior when enforcement capacity is reduced',
    'If coercion-driven: suppression can be reduced through administrative reform and enforcement relaxation. If identity-fused: compliance persists even after legal penalties are removed; constraint shifts from Snare to identity_locked geometry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_mobilization_coercion_intensity, empirical, 'Whether suppression is structural coercion or internalized identity obligation').

omega_variable(
    defense_necessity_vs_extraction,
    'What proportion of mobilization drag extractiveness is genuine war-production necessity versus institutional capture by military-industrial beneficiaries seeking rent extraction?',
    'Capacity analysis: comparison of actual defense output vs mobilization input ratios across NATO allies; cross-sectional analysis of wage/output relationships in defense vs civilian sectors; post-war assessment of whether production levels justified mobilization intensity',
    'If high necessity proportion: constraint is more Rope than Snare; beneficiaries'' extraction is legitimate coordination payment. If high rent-extraction proportion: constraint slides toward pure Snare with institutional capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(defense_necessity_vs_extraction, empirical, 'Ratio of genuine war necessity to extractive capture').

omega_variable(
    international_sunset_enforceability,
    'Will post-war international conditionality (EU accession, NATO integration, IMF programs) actually force demobilization and labor market normalization, or will wartime mobilization structures persist as zombie institutions post-conflict?',
    'Historical analysis of similar transitions: Balkans 1990s (Serbia, Bosnia), Caucasus conflicts (Georgia, Azerbaijan), post-WWII occupations. Assessment of international enforcement mechanisms and domestic political incentives for institutional continuity vs reform.',
    'If enforceable sunset: scaffold perspective is accurate, constraint has bounded temporal horizon. If non-enforceable: mobilization apparatus persists, constraint becomes piton or snare with permanent character.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(international_sunset_enforceability, empirical, 'Whether international frameworks will enforce post-war demobilization').

omega_variable(
    identity_locked_mobilization_dynamics,
    'Are mobilized workers'' identities fused with martial citizenship and collective defense identity in ways that make exit psychologically impossible even when legal barriers are removed?',
    'Post-war social cohesion studies; longitudinal tracking of demobilized workers'' labor force participation and wage paths; identity survey data on ''martial citizen'' self-concept persistence; comparison with non-mobilized cohorts',
    'If significant identity fusion: constraint persists post-war as identity-locked Rope rather than exiting to market Rope. Demobilized workers remain administratively subject to mobilization identity frames even in peace.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_mobilization_dynamics, empirical, 'Degree of identity fusion with martial citizenship and defense collective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukraine_labor_market_mobilization_drag, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uakmmd_tr_t0, ukraine_labor_market_mobilization_drag, theater_ratio, 0, 0.48).
narrative_ontology:measurement(uakmmd_tr_t6, ukraine_labor_market_mobilization_drag, theater_ratio, 6, 0.58).
narrative_ontology:measurement(uakmmd_tr_t12, ukraine_labor_market_mobilization_drag, theater_ratio, 12, 0.65).

% Extraction over time
narrative_ontology:measurement(uakmmd_be_t0, ukraine_labor_market_mobilization_drag, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(uakmmd_be_t6, ukraine_labor_market_mobilization_drag, base_extractiveness, 6, 0.48).
narrative_ontology:measurement(uakmmd_be_t12, ukraine_labor_market_mobilization_drag, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukraine_labor_market_mobilization_drag, resource_allocation).
narrative_ontology:affects_constraint(ukraine_labor_market_mobilization_drag, ukrainian_state_administrative_capacity).
narrative_ontology:affects_constraint(ukraine_labor_market_mobilization_drag, post_war_labor_market_reconstruction).

% DUAL FORMULATION NOTE:
% Mobilization drag downstream of state capacity and inflation control constraints; upstream from post-war labor market structure. The constraint family includes: (1) emergency mobilization authorization (ε ≈ 0.35, early Rope); (2) routine conscription enforcement (ε ≈ 0.58, Tangled Rope); (3) post-war identity-locked demobilization problem (ε ≈ 0.40+, identity_locked Rope or Snare depending on identity fusion degree).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ukraine_labor_market_mobilization_drag, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
