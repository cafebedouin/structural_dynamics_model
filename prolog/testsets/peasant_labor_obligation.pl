% ============================================================================
% CONSTRAINT STORY: peasant_labor_obligation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_peasant_labor_obligation, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: peasant_labor_obligation
 *   human_readable: Peasant Labor Obligation
 *   domain: feudal_political_economy
 *
 * SUMMARY:
 *   The peasant labor obligation in feudal systems represents a structural
 *   constraint on unfree populations requiring mandatory agricultural labor
 *   (corvée) or craft services to a feudal lord in exchange for land-use
 *   rights and purported protection. This constraint manifests differently
 *   across European regions, time periods (10th-16th centuries), and social
 *   contexts, but the core mechanism is consistent: a legally or customarily
 *   bound agricultural population cannot exit the obligation without
 *   abandoning subsistence access, while the lord captures coercive power to
 *   enforce labor duty. The constraint exhibits characteristics of a pure
 *   snare from the peasant perspective (high suppression, trapped exit,
 *   economic extraction) but appears as legitimate coordination (rope) from
 *   the lord's perspective and as degraded theater (piton) in late medieval
 *   periods when commercial alternatives eroded its functional necessity. The
 *   theater ratio increases over the interval as markets develop and formal
 *   coercion replaces economic necessity, suggesting the constraint was
 *   increasingly maintained through inertia rather than functional
 *   requirement.
 *
 * KEY AGENTS:
 *   - Peasant/Serf Population: Primary victim (powerless/trapped) — bears extraction cost of mandatory labor days; cannot exit without legal status change or dangerous flight; experiences maximum suppression through legal prohibition and coercive enforcement
 *   - Feudal Lord Class: Primary beneficiary (institutional/arbitrage) — captures labor surplus value; has discretion to demand additional services; benefits from labor monopoly without wage negotiation
 *   - Manor Court: Institutional actor (institutional/arbitrage) — maintains enforcement apparatus; increasingly theatrical in late medieval period as markets undermine economic necessity
 *   - Peasant Resistance Movements: Organized victims (organized/constrained) — through collective action (work slowdowns, guilds, religious networks) negotiate some benefit-sharing; demonstrate that pure snare classification underestimates peasant agency when organized
 *   - Market Actors: Emerging competitors (institutional/mobile) — wage labor and free tenancy represent alternative labor systems that gradually make obligation less economically necessary
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement ('this is how feudalism works') as immutable structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(peasant_labor_obligation, 0.62).
domain_priors:suppression_score(peasant_labor_obligation, 0.78).
domain_priors:theater_ratio(peasant_labor_obligation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(peasant_labor_obligation, extractiveness, 0.62).
narrative_ontology:constraint_metric(peasant_labor_obligation, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(peasant_labor_obligation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(peasant_labor_obligation, snare).
narrative_ontology:human_readable(peasant_labor_obligation, "Peasant Labor Obligation").
narrative_ontology:topic_domain(peasant_labor_obligation, "feudal_political_economy").

domain_priors:requires_active_enforcement(peasant_labor_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(peasant_labor_obligation, feudal_lord_class).
narrative_ontology:constraint_victim(peasant_labor_obligation, peasant_serf_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE BOUND SERF (SNARE) — Trapped by legal status and economic dependency. Cannot exit without abandoning land use rights, risking starvation. Bears full extraction cost of mandatory labor days; experiences maximum suppression through legal prohibition on exit and coercive enforcement.
constraint_indexing:constraint_classification(peasant_labor_obligation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: GENERATIONAL SERF HOUSEHOLD (SNARE) — Across generational time, the obligation is inherited with the land. Children born into obligation have no memory of alternative arrangement; the constraint naturalizes. Status shift to constrained only if escape to city becomes viable, which rarely occurs under feudal restrictions on movement.
constraint_indexing:constraint_classification(peasant_labor_obligation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: THE FEUDAL LORD (ROPE) — Experiences the constraint as coordination: obligation solves the problem of securing labor for demesne cultivation and infrastructure maintenance without market prices or wage negotiation. Efficient extraction mechanism perceived as legitimate exchange (labor for land-use rights and protection).
constraint_indexing:constraint_classification(peasant_labor_obligation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: PEASANT RESISTANCE MOVEMENTS (TANGLED ROPE) — Organized peasant actors (through guilds, religious networks, or coordinated work slowdowns) can negotiate reduced obligations or gain some benefit-sharing (collective access to mill, common pasture). Demonstrates that the pure snare classification underestimates peasant agency — coordination function emerges when victims organize.
constraint_indexing:constraint_classification(peasant_labor_obligation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: LATE MEDIEVAL MANOR COURT (PITON) — As markets developed and wage labor became available, the obligation persisted through institutional inertia. Manor courts maintained ritual recording and enforcement of labor obligations despite declining economic necessity. Theater ratio high (0.55) because much enforcement became performative — threat of penalty more powerful than actual enforcement as commercial alternatives eroded the constraint's functional necessity.
constraint_indexing:constraint_classification(peasant_labor_obligation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a distance, labor obligation appears as an immutable natural law of feudal economy: 'serfs are bound to the land,' 'this is how medieval society works,' 'extracting labor from the landed peasantry is simply the structure of things.' This perspective risks naturalizing what is actually a contingent institutional and legal arrangement backed by coercive enforcement. False summit: the engine will flag this as naturalization of a snare.
constraint_indexing:constraint_classification(peasant_labor_obligation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(peasant_labor_obligation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(peasant_labor_obligation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(peasant_labor_obligation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(peasant_labor_obligation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(peasant_labor_obligation, TR),
    TR >= 0.70.

:- end_tests(peasant_labor_obligation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The peasant is required to contribute 2-5 days per week of unpaid labor to the lord's demesne, equivalent to 20-35% of working time. This is pure extraction — labor provided without market wage compensation. The lord's claim to protection and land-use rights provides minimal actual benefit; the 'protection' clause is largely a cover story (lords often failed to protect against external threats). Extractiveness is not at maximum (0.72+) because peasants retain some negotiating power through slowdowns and collective resistance, and because regional variation exists (some obligations lighter than others). Suppression (0.78): Very high. Serf status is legally enforced; escape is prohibited by law and enforced through capture and return. Economic suppression is equally severe — loss of land-use rights means starvation; no alternative subsistence available in closed agricultural system. Suppression is not absolute (0.90+) because some peasants do escape to towns, and some resistance movements negotiate reductions. Theater ratio (0.55): Moderate-high. In early periods (T=0), enforcement is direct and material — the lord simply takes the labor he is owed. By late medieval period (T=400), with market alternatives available and peasant productivity declining from the original terms, much enforcement becomes symbolic (threat of penalty, recording in court rolls) rather than actual coercion. The theater increases because the constraint's functional necessity has eroded but institutional inertia maintains it.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence. From the peasant's trapped perspective, this is unambiguous snare — high extraction with escape impossible and suppression total. From the lord's arbitrage perspective, this is rope — labor coordination that enables demesne cultivation and subsistence production for the manor. From the perspective of organized peasant resistance movements (generational scope, constrained exit through bargaining), the classification shifts to tangled_rope — some agency emerges when victims organize, and genuine coordination functions appear alongside extraction. From the late medieval manor court's institutional position (high theater, declining economic necessity), the classification is piton — the ritual of obligation persists through inertia after its functional purpose has been displaced by market alternatives. The analytical observer risks the false summit of mountain (naturalization: 'feudal obligation is how medieval society works, an immutable structural feature'). The perspectival gaps expose that the constraint is contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   The peasant's directionality (d ≈ 0.95) derives from trapped exit + victim status + powerless position. No alternatives exist within the system; the agent bears full extraction; power is minimal. The sigmoid maps this to f(d) ≈ 1.42, producing high experienced chi. The lord's directionality (d ≈ 0.05) derives from arbitrage exit + beneficiary status + institutional position. The lord can walk away from individual obligations (use hired labor, convert to money rents) if needed; the agent benefits from extraction; power is maximum. The sigmoid maps this to f(d) ≈ -0.12, producing negative or minimal chi — the lord experiences no extraction from a constraint that benefits him. Organized peasant movements have d ≈ 0.50 (collective organization + constrained exit through negotiation creates some symmetry), mapping to f(d) ≈ 0.65 and moderate chi. This structural divergence is the core analytical content: same constraint, radically different directionality values, producing different classifications from different perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the perspectival divergence. No single classification is 'correct' — the constraint is legitimately rope from the lord's perspective (solves labor coordination problem), legitimately snare from the peasant's perspective (pure extraction with no exit), legitimately tangled_rope from the organized peasant movement's perspective (mixed coordination and extraction with emergent agency), and legitimately piton from the late medieval analytical perspective (degraded through market competition but maintained by inertia). The false summit (mountain) reveals the analytical risk: naturalization of the constraint as 'just how feudal systems work' disguises its contingency on legal coercion and economic closure. Once legal status changes and markets develop, the constraint collapses — not because law discovered something about nature, but because the institutional arrangement lost enforcement apparatus and economic necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_vs_customary_binding,
    'Is peasant obligation binding through formal legal status (serf/villain classification) or through customary practice and economic dependency?',
    'Documentary analysis of charters, manorial records, and law codes; comparison of enforceability in manors with written customs vs. oral tradition; examination of whether peasants could legally claim freedom through departure if legal status was uncertain.',
    'If primarily legal: the constraint is enforceable only through judicial coercion and collapses when legal frameworks change. If primarily customary: the constraint persists through social internalization even after legal status is abolished, suggesting identity_locked mechanisms (not just trapped legal status).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_vs_customary_binding, empirical, 'Whether obligation is legally or customarily binding').

omega_variable(
    peasant_identity_fusion,
    'To what extent did bound peasants internalize their obligation as part of identity (identity_locked) versus experiencing it as external coercion (trapped)?',
    'Analysis of peasant oral narratives, legal disputes, and post-emancipation behavior; examination of whether peasants resisted the status itself or negotiated better terms within it; observation of peasant movement patterns after legal obligation ended.',
    'If high identity fusion: classification should emphasize identity_locked exit option, suggesting deeper structural binding than legal status alone. Peasants would continue binding-like behavior even after law changes. If low fusion: classification is purely trapped, and abolition removes the constraint completely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(peasant_identity_fusion, empirical, 'Degree of peasant identity fusion with serf status').

omega_variable(
    subsistence_margin_sufficiency,
    'Did the land allocated to peasants as compensation for labor obligation provide subsistence security or permanent poverty trap?',
    'Agronomic analysis of plot yields vs household consumption requirements; comparison of peasant nutritional and housing standards across regions and time periods; examination of whether peasant savings could accumulate or were systematized toward zero through tithe, mill monopoly, and taxation.',
    'If subsistence sufficient: the obligation is Tangled Rope (genuine coordination with extraction overlay). If subsistence insufficient and enforced zero-sum: the obligation is pure Snare (extraction mechanism prevents capital formation for exit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subsistence_margin_sufficiency, empirical, 'Whether land allocation provided subsistence security').

omega_variable(
    alternative_labor_systems_viability,
    'Were wage labor or free tenant systems technologically or economically viable alternatives, or was obligation the only feasible labor solution for medieval agriculture?',
    'Historical comparison of regions and time periods where different labor systems coexisted; economic analysis of transaction costs for wage vs obligation systems; examination of why free tenancy expanded in some regions while obligation remained in others.',
    'If viable alternatives existed but obligation was chosen for extraction: snare classification confirmed. If obligation was the only system that solved medieval labor coordination: classification shifts toward rope (legitimate coordination solution to pre-market labor scarcity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_labor_systems_viability, empirical, 'Viability of alternative labor systems in medieval context').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(peasant_labor_obligation, 0, 400).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plo_tr_t0, peasant_labor_obligation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(plo_tr_t200, peasant_labor_obligation, theater_ratio, 200, 0.4).
narrative_ontology:measurement(plo_tr_t400, peasant_labor_obligation, theater_ratio, 400, 0.55).

% Extraction over time
narrative_ontology:measurement(plo_be_t0, peasant_labor_obligation, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(plo_be_t200, peasant_labor_obligation, base_extractiveness, 200, 0.62).
narrative_ontology:measurement(plo_be_t400, peasant_labor_obligation, base_extractiveness, 400, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(peasant_labor_obligation, resource_allocation).
narrative_ontology:affects_constraint(peasant_labor_obligation, feudal_land_monopoly).
narrative_ontology:affects_constraint(peasant_labor_obligation, mill_tithe_monopoly).
narrative_ontology:affects_constraint(peasant_labor_obligation, serfdom_legal_status).

% DUAL FORMULATION NOTE:
% The peasant labor obligation is downstream of legal serfdom status (which creates the enforceability mechanism) and the feudal land monopoly (which creates economic necessity for the obligation). Each constraint has its own epsilon reflecting different measurement perspectives: serfdom captures legal binding mechanisms (ε=0.45), land monopoly captures economic closure (ε=0.58), labor obligation captures the actual extraction flow (ε=0.62). The three constraints together form a reinforcing system; decomposition enables precise structural analysis of where the binding actually comes from.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
