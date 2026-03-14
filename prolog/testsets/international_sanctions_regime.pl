% ============================================================================
% CONSTRAINT STORY: international_sanctions_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_international_sanctions_regime, []).

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
 *   constraint_id: international_sanctions_regime
 *   human_readable: International Sanctions Regime as Coordinated Extraction
 *   domain: geopolitical/economic_coercion
 *
 * SUMMARY:
 *   International sanctions regimes represent a structured mechanism for
 *   coordinating geopolitical coercion across multiple states against
 *   designated targets. The regime exhibits simultaneous coordination
 *   (coalition states align on enforcement, third parties face predictable
 *   trade restrictions) and extraction (sanctioned populations bear
 *   humanitarian costs, third-party trade partners face secondary effects,
 *   enforcement infrastructure persists despite policy failure). The
 *   constraint operates across three distinct institutional hierarchies — the
 *   sanctioning coalition, the global trade system, and the internal dynamics
 *   of sanctioned states — each experiencing different extractiveness
 *   depending on structural position. The empirical trajectory shows
 *   increasing theater ratio: as sanctions fail to produce stated policy
 *   changes, enforcement infrastructure expands to document compliance rather
 *   than achieve behavioral change, suggesting a transition from coordination
 *   mechanism to degraded institutional performance.
 *
 * KEY AGENTS:
 *   - Sanctioning Coalition States: Primary beneficiaries (institutional/arbitrage) — extract geopolitical leverage and market advantages; maintain exit option through threat of sanctions lifting
 *   - Sanctioned State Government: Secondary beneficiary and victim (organized/constrained) — extracting internal legitimacy through nationalist mobilization while bearing economic extraction
 *   - Sanctioned Civilian Population: Primary victim (powerless/trapped) — bears extraction through currency collapse, medicine shortages, economic contraction with no exit mechanism
 *   - Third-Party Trade Partners: Secondary victims and partial beneficiaries (moderate/constrained) — constrained by secondary sanctions but benefit from reduced competition and coordination framework
 *   - Humanitarian and Mediation Organizations: Organized agents (organized/constrained) — perceive sanctions as temporary coercive mechanism with sunset logic toward negotiation
 *   - Sanctions Enforcement Infrastructure: Institutional actors (institutional/arbitrage) — maintain regime through inertia; transform into performative compliance theater as policy objectives fail
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — observes historical pattern of weak efficacy and rising theater ratio, suggesting institutional decay rather than ongoing coordination function
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(international_sanctions_regime, 0.58).
domain_priors:suppression_score(international_sanctions_regime, 0.68).
domain_priors:theater_ratio(international_sanctions_regime, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(international_sanctions_regime, extractiveness, 0.58).
narrative_ontology:constraint_metric(international_sanctions_regime, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(international_sanctions_regime, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(international_sanctions_regime, tangled_rope).
narrative_ontology:human_readable(international_sanctions_regime, "International Sanctions Regime as Coordinated Extraction").
narrative_ontology:topic_domain(international_sanctions_regime, "geopolitical/economic_coercion").

domain_priors:requires_active_enforcement(international_sanctions_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(international_sanctions_regime, sanctioning_coalition_states).
narrative_ontology:constraint_beneficiary(international_sanctions_regime, enforcement_infrastructure_actors).
narrative_ontology:constraint_victim(international_sanctions_regime, sanctioned_state_population).
narrative_ontology:constraint_victim(international_sanctions_regime, third_party_trade_partners).
narrative_ontology:constraint_victim(international_sanctions_regime, humanitarian_access_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SANCTIONED CIVILIAN POPULATION (SNARE) — Bears extraction with no exit mechanism. Sanctions regime nominally targets state behavior but operationally extracts from civilians through currency collapse, medicine shortages, food import restrictions, and economic contraction. No meaningful alternative — citizenship is fixed, exit is prohibited, and the constraint persists across regime changes.
constraint_indexing:constraint_classification(international_sanctions_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THIRD-PARTY TRADE PARTNERS (TANGLED ROPE) — Constrained by secondary sanctions and compliance costs, but also benefit from reduced competition in sanctioned markets and from the coordination framework itself (predictable trade rules). High costs of exit (losing market access, facing sanctions exposure) but some genuine coordination function in that the regime creates stable expectations about geopolitical risk.
constraint_indexing:constraint_classification(international_sanctions_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SANCTIONING COALITION STATES (ROPE) — Primary beneficiaries with arbitrage options. Sanctions enable geopolitical leverage without military intervention. Coalition members can threaten to lift sanctions (exit mechanism) or coordinate to tighten them. Net beneficiary — extracts compliance signals and market advantages while maintaining exit option.
constraint_indexing:constraint_classification(international_sanctions_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMANITARIAN AND MEDIATION ORGANIZATIONS (SCAFFOLD) — Organized actors (UN, ICRC, diplomatic mediators) perceive sanctions as a temporary coercive mechanism with sunset potential: escalation ladder toward negotiation. The constraint has a functional sunset: sanctions are meant to force behavior change and negotiation, after which they should lift. Real sunset clauses exist in some regimes (Iran JCPOA precedent). Extraction is tolerated because the mechanism has explicit exit logic.
constraint_indexing:constraint_classification(international_sanctions_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SANCTIONS ENFORCEMENT INFRASTRUCTURE (PITON) — Treasury departments, compliance bureaus, and enforcement agencies maintain sanctions regimes through institutional inertia. Original function (coerce specific behavior change) often atrophies into performative compliance theater: elaborate documentation of sanctions avoidance that consumes resources but generates minimal policy change. Theater ratio high because enforcement persists even when sanctions fail to achieve stated objectives.
constraint_indexing:constraint_classification(international_sanctions_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: SANCTIONED STATE GOVERNMENT (TANGLED ROPE) — Constrained by sanctions but also experiences coordination benefit: sanctions unite the regime politically through nationalist mobilization and justify emergency powers. The regime faces high extraction (economic loss, reduced capacity) but gains internal coordination benefit (legitimacy, suppression justification, reduced internal opposition). Exit requires capitulation or military victory — both high-cost options.
constraint_indexing:constraint_classification(international_sanctions_regime, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / HISTORICAL VIEW (PITON) — From a civilizational perspective, economic sanctions as a coercion mechanism show weak efficacy: broad sanctions rarely produce intended behavior change within reasonable timeframes. The regime persists through institutional inertia despite empirical failure. Sanctions are maintained because they are politically easier than alternative coercive mechanisms (military), not because they work. Theater ratio rises as sanctions enforcement becomes decoupled from policy outcomes.
constraint_indexing:constraint_classification(international_sanctions_regime, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(international_sanctions_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(international_sanctions_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(international_sanctions_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(international_sanctions_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(international_sanctions_regime, TR),
    TR >= 0.70.

:- end_tests(international_sanctions_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting genuine extraction from civilian populations and humanitarian systems alongside real coordination functions for the sanctioning coalition and third-party trade systems. The value increased from 0.35 to 0.62 over the measurement interval, indicating extraction accumulation and mission creep: initial sanctions focused on specific behavioral objectives (nuclear program, human rights violations) but expanded to broader economic strangulation. Suppression (0.68): High. Sanctions create significant barriers to exit: sanctioned populations cannot emigrate easily (visa restrictions), cannot access alternative markets (global enforcement of secondary sanctions), and cannot appeal to international bodies (permanent security council veto). Third-party trade partners face reputational and legal penalties for sanctions evasion. Theater ratio (0.61): Moderate-high, reflecting growing documentary burden of sanctions compliance that no longer correlates with policy outcomes. The measurement trajectory shows theater ratio rising faster than extractiveness, suggesting institutional drift toward performative enforcement. Claimed type (Tangled Rope) derives from genuine coordination function (coalition maintenance, trade predictability) combined with asymmetric extraction (civilian populations bear costs disproportionate to policy efficacy).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates inter-institutional perspectival heterogeneity at multiple scales. At the state level: sanctioning coalition vs sanctioned state experience opposite extractiveness directions (beneficiary vs victim). At the population level: sanctioning coalition citizens experience minimal extraction (they pay enforcement costs, gain geopolitical leverage) while sanctioned populations experience maximum extraction (they pay humanitarian costs, gain no leverage). At the trade system level: major trade partners can arbitrage (diversify suppliers, circumvent legally), while dependent trade partners cannot (single-supplier vulnerability, limited alternatives). The perspectival gap is not primarily cognitive (different beliefs about the same constraint) but structural (each agent's position in the enforcement hierarchy determines their experienced extractiveness independently of belief). This gap is diagnostic: if all perspectives produced the same classification type, the constraint would be Mountain-like (binding on everyone equally). The fact that perspectives diverge across Snare/Rope/Tangled Rope/Scaffold/Piton reveals that the constraint's force is asymmetrically distributed across the institutional hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by beneficiary/victim status and exit options. Sanctioning coalition states: beneficiaries + arbitrage → d ≈ 0.15 → f(d) ≈ -0.01 → negative or minimal χ (they experience the constraint as coordination, not extraction). Sanctioned civilian population: victims + trapped → d ≈ 0.95 → f(d) ≈ 1.42 → maximum χ (they experience full extraction). Third-party trade partners: mixed (partial victims) + constrained → d ≈ 0.65 → f(d) ≈ 1.00 → moderate χ. Sanctioned state government: ambiguous (both victim and beneficiary) + constrained → d ≈ 0.55 → f(d) ≈ 0.75 → moderate χ with internal asymmetry. Enforcement infrastructure: beneficiaries + arbitrage → d ≈ 0.15 → f(d) ≈ -0.01 → negative χ, but Piton classification overrides (theater_ratio ≥ 0.70 drives Piton gate regardless of χ). The computational chain produces divergent classifications from the same base_properties, which is the intended output: Tangled Rope from the canonical analytical context (moderate power/biographical/mobile/national) vs Snare from powerless perspective vs Rope from institutional beneficiary perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The mandatrophy is resolved by recognizing that sanctions regimes are hybrid mechanisms combining genuine coordination (coalition alignment, predictable trade rules, escalation ladder toward negotiation) with asymmetric extraction (humanitarian costs borne by non-agents). The Tangled Rope classification preserves both functions: the regime coordinates international responses to violations while simultaneously extracting from civilian populations who did not authorize the violation and cannot exit the constraint. The false mountain temptation is to naturalize sanctions as 'necessary tools of statecraft' or 'inevitable accompaniment to international law enforcement,' which would imply immutable ε ≤ 0.25. Instead, the measured extractiveness (0.58) reflects that the humanitarian cost is discretionary — alternative coercive mechanisms exist, alternatives that target decision-makers more precisely rather than dispersing costs across civilian populations. The measurement trajectory (extractiveness rising from 0.35 to 0.62, theater rising from 0.42 to 0.68) supports classification as active extraction mechanism, not immutable law. The analytical observer's Piton classification correctly identifies institutional drift: as sanctions fail to produce stated objectives, enforcement infrastructure persists through momentum rather than function, and enforcement theater rises to justify the continued existence of the infrastructure. This is not a contradiction — Piton is a valid classification indicating degradation, not a misclassification. The regime exhibits genuine coordination early (time_point 0, lower theater) and increasing theater later (time_point 8, higher theater), consistent with institutional decay pattern. Mandatrophy is fully resolved: Tangled Rope is appropriate because coordination and extraction genuinely coexist at different institutional levels, not because we cannot distinguish them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavior_change_attribution,
    'When sanctions regimes do coincide with behavior change, is the change causally attributable to sanctions pressure or to independent factors (coalition breakup, leadership change, military defeat)?',
    'Counterfactual case analysis comparing sanctioned states with non-sanctioned states undergoing similar regime transitions; isolation of causal paths through structural modeling',
    'If sanctions causally efficacious: Rope classification dominates (legitimate coordination for behavior change). If causally inert: Snare classification dominates (pure extraction without coordination function).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(behavior_change_attribution, empirical, 'Causal attribution of behavior change to sanctions pressure').

omega_variable(
    humanitarian_cost_threshold,
    'At what humanitarian cost threshold do sanctions transition from legitimate coercion to extraction targeting civilians?',
    'Epidemiological studies of excess mortality, health outcomes, and malnutrition in sanctioned populations; comparison with non-sanctioned population controls; cost-benefit analysis against stated policy objectives',
    'If threshold exceeded early: Snare classification shifts upward (higher extractiveness). If threshold rarely or never exceeded: Tangled Rope classification confirmed (extraction balanced by coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_cost_threshold, empirical, 'Humanitarian cost threshold distinguishing coercion from civilian extraction').

omega_variable(
    secondary_sanctions_compliance_opacity,
    'What proportion of third-party compliance with secondary sanctions is genuine adherence vs. theatrical documentation of circumvention attempts?',
    'Financial flow analysis tracking actual trade diversion; audit studies of sanctions-avoidance compliance documentation; comparison of declared vs actual trade patterns',
    'If high compliance: theater_ratio lower (genuine enforcement). If high documentation theater: theater_ratio higher (Piton classification stronger, institutional inertia dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_sanctions_compliance_opacity, empirical, 'Compliance with secondary sanctions vs. circumvention theater').

omega_variable(
    coalition_heterogeneity_regime_stability,
    'Does sanctioning coalition heterogeneity (different members, different enforcement intensity, different exit threshold preferences) produce extractive coalition maintenance separate from stated policy objectives?',
    'Game-theoretic analysis of coalition payoffs; historical analysis of sanctions lifting/tightening decisions relative to stated behavioral objectives vs. coalition maintenance incentives',
    'If coalition maintenance dominates: extractiveness increases (sanctions persist despite policy failure because members benefit from coordination lock). If policy objectives dominant: extractiveness reflects genuine coercion attempt.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_heterogeneity_regime_stability, conceptual, 'Whether coalition maintenance creates independent extraction incentive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(international_sanctions_regime, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sanctions_tr_t0, international_sanctions_regime, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sanctions_tr_t2, international_sanctions_regime, theater_ratio, 2, 0.52).
narrative_ontology:measurement(sanctions_tr_t5, international_sanctions_regime, theater_ratio, 5, 0.61).
narrative_ontology:measurement(sanctions_tr_t8, international_sanctions_regime, theater_ratio, 8, 0.68).

% Extraction over time
narrative_ontology:measurement(sanctions_be_t0, international_sanctions_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sanctions_be_t2, international_sanctions_regime, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(sanctions_be_t5, international_sanctions_regime, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(sanctions_be_t8, international_sanctions_regime, base_extractiveness, 8, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(international_sanctions_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(international_sanctions_regime, secondary_sanctions_compliance).
narrative_ontology:affects_constraint(international_sanctions_regime, humanitarian_access_restrictions).
narrative_ontology:affects_constraint(international_sanctions_regime, nationalist_mobilization_regimes).

% DUAL FORMULATION NOTE:
% The international sanctions regime can be decomposed into three distinct constraint stories: (1) coalition coordination mechanism (lower ε, Rope focus), (2) humanitarian access restriction (higher ε, Snare focus), and (3) enforcement infrastructure institutional inertia (theater focus, Piton). This story models the hybrid form. Upstream dependencies: the regime presupposes a functioning global trade system and international legal framework for enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(international_sanctions_regime, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
