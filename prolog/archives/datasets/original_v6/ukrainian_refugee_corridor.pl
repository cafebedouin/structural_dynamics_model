% ============================================================================
% CONSTRAINT STORY: ukrainian_refugee_corridor
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ukrainian_refugee_corridor, []).

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
 *   constraint_id: ukrainian_refugee_corridor
 *   human_readable: Ukrainian Refugee Corridor: Coordination vs. Exploitation
 *   domain: humanitarian/geopolitical
 *
 * SUMMARY:
 *   The Ukrainian refugee corridor represents a structural constraint that
 *   simultaneously coordinates humanitarian evacuation and enables
 *   exploitation. Since 2022, an estimated 6+ million Ukrainians have fled
 *   active conflict, predominantly through Poland, Moldova, and Romania into
 *   EU member states. The corridor exhibits the diagnostic signature of
 *   tangled rope at the analytical level: genuine coordination function
 *   (life-saving border logistics, asylum processing, medical care) is
 *   interlocked with asymmetric extraction (labor market arbitrage, smuggling
 *   profiteering, documented trafficking, rule-of-law degradation).
 *   Extractiveness has increased over the measurement interval (0.42 → 0.58)
 *   as initial humanitarian response gave way to routinized processing,
 *   smuggling networks consolidated, and host nations maximized labor market
 *   gains while minimizing integration costs. Theater ratio (0.48) reflects
 *   that humanitarian messaging and legal asylum frameworks mask commercial
 *   extraction mechanisms. The constraint operates at regional scale through
 *   multiple institutional actors (transit nations, EU bureaucracy, NGOs,
 *   criminal networks) but its primary victims — individual refugees —
 *   experience it as a snare: trapped between conflict and an exploitative
 *   safe passage system, with suppressed alternatives and no meaningful exit.
 *
 * KEY AGENTS:
 *   - Ukrainian Refugees: Primary victims (powerless/trapped) — face binary choice: remain in conflict or navigate heavily-monitored corridor with smuggling costs, documentation barriers, trafficking risk, and labor market underemployment
 *   - Transit Nations (Poland, Moldova, Romania): Primary beneficiaries and hybrid actors (moderate/constrained) — coordinate humanitarian logistics while extracting through: labor supply subsidies, transit fees, EU funding rewards for corridor maintenance, border control leverage over refugees
 *   - EU Institutional Framework: Secondary beneficiary (institutional/arbitrage) — maintains liberal asylum posture while shifting burden to border states; benefits from labor supply; can arbitrage responsibility to individual member states
 *   - Smuggling Networks and Traffickers: Organized extractors (organized/constrained) — pure extraction mechanism with pricing power over desperate refugees; operate in legal gray zones with low law-enforcement pressure
 *   - NGO Humanitarian Networks: Hybrid coordinators (organized/constrained) — deliver genuine services (medical, legal, family tracing) while embedded in extraction: dependent on government permits, restricted access to trafficking victims, institutionalized mission creep
 *   - International Refugee Convention Framework: Institutional ritual (institutional/arbitrage) — formal legal structure operates performatively; provides legitimacy for extraction rather than functional protection; maintained through geopolitical interest, not refugee outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ukrainian_refugee_corridor, 0.58).
domain_priors:suppression_score(ukrainian_refugee_corridor, 0.72).
domain_priors:theater_ratio(ukrainian_refugee_corridor, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ukrainian_refugee_corridor, extractiveness, 0.58).
narrative_ontology:constraint_metric(ukrainian_refugee_corridor, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(ukrainian_refugee_corridor, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ukrainian_refugee_corridor, tangled_rope).
narrative_ontology:human_readable(ukrainian_refugee_corridor, "Ukrainian Refugee Corridor: Coordination vs. Exploitation").
narrative_ontology:topic_domain(ukrainian_refugee_corridor, "humanitarian/geopolitical").

domain_priors:requires_active_enforcement(ukrainian_refugee_corridor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ukrainian_refugee_corridor, transit_nations_logistics_revenue).
narrative_ontology:constraint_beneficiary(ukrainian_refugee_corridor, smuggling_networks).
narrative_ontology:constraint_beneficiary(ukrainian_refugee_corridor, labor_market_arbitrage_countries).
narrative_ontology:constraint_victim(ukrainian_refugee_corridor, ukrainian_refugees).
narrative_ontology:constraint_victim(ukrainian_refugee_corridor, host_nation_public_services).
narrative_ontology:constraint_victim(ukrainian_refugee_corridor, transit_corridor_rule_of_law).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN REFUGEE (SNARE) — Faces binary choice: remain in active conflict zone or navigate heavily-monitored corridor with documentation barriers, smuggling costs, and human trafficking risk. Cannot exit the constraint itself; all alternatives lead through it or toward greater danger. Bears full extraction costs: pays smugglers, loses savings, accepts exploitation, experiences profound suppression of alternatives.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: TRANSIT NATION (TANGLED ROPE) — Coordinates legitimate humanitarian logistics (border crossing infrastructure, aid distribution, legal asylum processing) while benefiting from extraction: refugees pay transit fees, labor supply subsidizes economies, EU funding rewards corridor maintenance. High suppression of alternatives (closed borders increase leverage), active enforcement (militia presence, document checks). Constrained exit — cannot refuse corridor without geopolitical cost, cannot fully profiteer without legitimacy loss.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EU INSTITUTIONAL FRAMEWORK (ROPE) — Coordinates humanitarian asylum system (Temporary Protection Directive, burden-sharing mechanisms) with minimal coercive overhead. Benefits from refugee labor supply, maintains liberal geopolitical posture, achieves coordination of member-state policy without heavy-handed enforcement. Arbitrage available — can shift burden to Poland/Hungary, reframe border responsibility. Extraction runs toward this agent; perceives constraint as successful coordination.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: NGO HUMANITARIAN NETWORKS (TANGLED ROPE) — Organized agents (IOM, UNHCR, local aid groups) coordinate genuine refugee services (medical care, legal assistance, family tracing) while embedded in extraction mechanisms: dependency on government permits, restricted access to smuggling victims, mission creep that substitutes for state responsibility. Constrained exit — cannot withdraw without abandoning refugees, cannot fully resist institutionalization. Both genuine coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: SMUGGLING NETWORKS (SNARE) — Highly organized extraction mechanism with minimal coordination function. Primary business is capturing refugee desperation through pricing power, falsified documents, unsafe transport, and trafficking. Constrained rather than trapped due to organizational capacity and law enforcement risk. Maximizes extraction through suppression: isolation of refugees, false information, threats, document confiscation.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL REFUGEE CONVENTION (PITON) — The formal legal framework (1951 Convention, subsequent protocols) is substantially performative. Written to address mid-20th century displacement, it lacks mechanisms for mass conflict-driven migration, fails to distinguish refugees from migrants in practice, and is maintained through ritual invocation rather than functional enforcement. Theater high (0.48 reflects that humanitarian messaging substitutes for accountability). The framework persists through institutional inertia and geopolitical interest, not because it effectively protects refugees.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, the refugee corridor simultaneously coordinates legitimate humanitarian response (life-saving logistics) and enables extraction (labor market arbitrage, documented abuse, smuggling profiteering, rule-of-law degradation). Both functions are real and interlocked. The constraint exists because rich nations benefit from controlled refugee flow and cheap labor; the humanitarian function justifies the extraction mechanism. This is not a natural law — it is a political structure that masks extraction as coordination.
constraint_indexing:constraint_classification(ukrainian_refugee_corridor, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ukrainian_refugee_corridor_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ukrainian_refugee_corridor, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ukrainian_refugee_corridor, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ukrainian_refugee_corridor, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ukrainian_refugee_corridor, TR),
    TR >= 0.70.

:- end_tests(ukrainian_refugee_corridor_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from refugees through: smuggling costs (average $2,000-5,000 per person), opportunity cost of delayed employment (6-12 months for work permits), wage underemployment relative to skills (refugees earning 30-40% below native baseline for 2+ years), and documented exploitation in domestic work and agricultural sectors. The extraction is not as severe as pure snare (0.72+) because some refugees do successfully transit and eventually integrate; coordination function is real (border processing, legal asylum, aid distribution). Suppression (0.72): High. Strong barriers to alternatives: conflict zone is lethal (forced choice), border crossing heavily monitored (document requirements, fingerprinting), smuggling is life-threatening (unsafe transport, trafficking risk), documentation standards are degraded (facilitates both trafficking and labor exploitation), and destination nations maintain restrictive onward migration rules. Theater ratio (0.48): Moderate. The humanitarian messaging is not purely performative — real services (medical care, legal assistance, food distribution) are delivered. But institutional overhead is substantial: paperwork and processing substitutes for immediate protection, funding tracking and donor accountability competes with refugee outcomes, and legal asylum frameworks create bureaucratic delays while extraction mechanisms operate unregulated.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between refugee and transit nation is the core diagnostic signature. Refugees classify the corridor as snare (pure extraction, trapped, maximum suppression) while transit nations classify it as tangled rope (coordination + benefit, constrained but organized, moderate suppression). This gap reveals the structural ambiguity: the corridor genuinely coordinates humanitarian evacuation AND genuinely extracts value from refugees. No single perspective sees the full structure. The refugee's perspective captures the extraction but misses the real humanitarian function (without the corridor, refugees face lethal alternatives). The transit nation's perspective captures the coordination but downplays the suppression and exploitation (documentation requirements, labor market segregation, trafficking tolerance). The EU's rope perspective misses that arbitrage-shifting burden to border states is a form of institutional coercion. The smuggling network's snare perspective is inverted: they are the extractors, not victims. The NGO networks perceive themselves as pure coordinators but are tangled rope: delivering services while constrained by state apparatus from addressing trafficking. The international legal framework perceives rope but is piton: institutional ritual performing protection while extraction mechanisms operate unregulated.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's d value (directionality: 0.0=full beneficiary, 1.0=full target) is derived from the agent's structural position: power level, exit options, and beneficiary/victim status. Powerless/trapped refugees: d ≈ 0.95 (full target). The sigmoid f(0.95) ≈ 1.42 amplifies experienced extractiveness. Transit nations (moderate/constrained, both beneficiary and victim): d ≈ 0.50 (symmetric cost-benefit), f(0.50) ≈ 0.65. EU institutional (institutional/arbitrage, beneficiary): d ≈ 0.15 (shifted toward beneficiary), f(0.15) ≈ -0.01. Smuggling networks (organized/constrained, extractors): d ≈ 0.65 (target of law enforcement), f(0.65) ≈ 1.00, but they experience this as profit rather than extraction cost. NGO networks (organized/constrained, victims of institutional constraints): d ≈ 0.55, f(0.55) ≈ 0.75. International refugee framework (institutional/arbitrage, beneficiary through legitimation): d ≈ 0.20, f(0.20) ≈ 0.02. The analytical observer (analytical/analytical, unbiased): d ≈ 0.72 (observing extraction flow), f(0.72) ≈ 1.15.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy (classification ambiguity above ε > 0.70) is partially resolved through perspectival decomposition. From the refugee perspective, the corridor is snare (pure extraction, ε ≈ 0.75, suppression 0.85). From the transit nation perspective, it is tangled rope (coordination + extraction, ε ≈ 0.50, suppression 0.60). From the analytical perspective, it is tangled rope (coordinating evacuation while enabling extraction, ε ≈ 0.58). The constraint does not exceed ε > 0.70 overall because genuine humanitarian coordination is real and substantive — this is not extractive predation disguised as aid. However, the refugee perspective's snare classification reveals that from the most vulnerable agent's position, experienced extraction IS maximal. The resolution is not 'pick the true type' but recognize that both snare and tangled rope are structurally accurate from their respective contexts. The constraint's existence is justified (life-saving humanitarian function); its extraction is enabled by asymmetric power (refugees cannot exit; transit nations can). The mandatrophy resolves by showing that the constraint performs genuine coordination AND extracts value, making tangled rope the appropriate classification at the analytical level, while acknowledging that from the refugee's perspective, the extraction dominates the experienced constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trafficking_versus_smuggling_boundary,
    'What proportion of the measured suppression (0.72) is coercive trafficking (agent lacks agency) versus consensual smuggling (agent pays for services knowing risks)?',
    'Post-arrival interviews with escaped/settled refugees; comparison of documented trafficking cases vs. refugee-reported smuggling transactions; forensic analysis of coercion mechanisms in specific corridor networks',
    'High trafficking proportion: snare classification confirmed from refugee perspective, suppression justified. High smuggling proportion: tangled rope classification gains force (refugees have agency in transaction), extraction is voluntary exchange at high cost, not pure coercion. Changes victim narrative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trafficking_versus_smuggling_boundary, empirical, 'Trafficking coercion vs. consensual smuggling proportion').

omega_variable(
    host_nation_capacity_versus_exploitation,
    'Is high suppression (0.72) in transit nations driven by genuine capacity limits (infrastructure, social services, housing) or by deliberate gatekeeping to extract labor and limit onward migration?',
    'Comparative analysis: capacity investment trajectory in transit vs. destination nations; correlation between corridor strictness and labor market demand; policy documents revealing intent',
    'If capacity-driven: suppression is coordination cost, not extraction; tangled rope classification is correct. If gatekeeping-driven: suppression is deliberate mechanism to extract labor; snare classification from host-nation-public-services victim perspective is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(host_nation_capacity_versus_exploitation, empirical, 'Suppression source: capacity limits vs. deliberate extraction').

omega_variable(
    alternative_corridor_feasibility,
    'Would direct refugee resettlement from Ukraine (bypassing transit nations) be feasible at similar scale and cost, or is the regional corridor structure necessary?',
    'Capacity analysis of destination nations; cost comparison of direct resettlement vs. transit-corridor processing; precedent from other mass displacement events',
    'If direct resettlement feasible: current corridor structure is chosen for extraction benefits (transit nation profit, labor arbitrage), not necessity; snare classification gains force. If infeasible: corridor is least-harm coordination under constraints; tangled rope classification is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_corridor_feasibility, empirical, 'Whether direct resettlement is a feasible alternative').

omega_variable(
    rule_of_law_recovery_trajectory,
    'Post-conflict, will corridor infrastructure (smuggling networks, degraded documentation standards, normalized exploitation) persist and compromise legitimate refugee systems?',
    'Historical analysis of post-conflict refugee corridors (Bosnia, Syria, Afghanistan); trajectory of smuggling networks after conflict ends; institutional persistence of compromised border practices',
    'If trajectory shows persistence: the extraction mechanism (suppression, corruption, trafficking) is durable; classifying as pure tangled rope (reversible) underestimates long-term harm. Reclassify as partial snare with civilizational time horizon.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rule_of_law_recovery_trajectory, empirical, 'Whether extraction mechanisms persist post-conflict').

omega_variable(
    refugee_labor_market_benefit_duration,
    'Do host nations that receive Ukrainian refugees experience net economic benefit (labor supply, tax contribution, entrepreneurship) that outweighs social costs, or is the benefit short-lived arbitrage?',
    '10-year longitudinal economic analysis of refugee-receiving nations; wage trajectory comparison with native labor; fiscal contribution analysis; social cost accounting',
    'If net positive long-term: extraction narrative overstated; rope classification (genuine coordination) gains force. If short-term arbitrage: extraction narrative confirmed; snare classification from labor victim perspective is valid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refugee_labor_market_benefit_duration, empirical, 'Whether refugee labor benefits host nations long-term').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ukrainian_refugee_corridor, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(urc_tr_t0, ukrainian_refugee_corridor, theater_ratio, 0, 0.35).
narrative_ontology:measurement(urc_tr_t6, ukrainian_refugee_corridor, theater_ratio, 6, 0.42).
narrative_ontology:measurement(urc_tr_t12, ukrainian_refugee_corridor, theater_ratio, 12, 0.48).

% Extraction over time
narrative_ontology:measurement(urc_be_t0, ukrainian_refugee_corridor, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(urc_be_t6, ukrainian_refugee_corridor, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(urc_be_t12, ukrainian_refugee_corridor, base_extractiveness, 12, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ukrainian_refugee_corridor, resource_allocation).
narrative_ontology:affects_constraint(ukrainian_refugee_corridor, ukrainian_labor_market_integration).
narrative_ontology:affects_constraint(ukrainian_refugee_corridor, eu_migration_burden_sharing).
narrative_ontology:affects_constraint(ukrainian_refugee_corridor, human_trafficking_networks).

% DUAL FORMULATION NOTE:
% The refugee corridor constraint family decomposes into three structurally distinct stories: (1) humanitarian coordination (ε ≈ 0.30, rope/tangled rope) focused on border crossing logistics and asylum processing; (2) labor market extraction (ε ≈ 0.65, snare/tangled rope) focused on wage suppression and employment segregation; (3) trafficking networks (ε ≈ 0.80, snare) focused on smuggling profiteering and coercive debt mechanisms. This story integrates all three while emphasizing the entanglement between coordination and extraction. The upstream constraint (conflict in Ukraine) drives the corridor; the downstream constraints (labor market integration, trafficking) are consequences of the corridor's extraction mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ukrainian_refugee_corridor, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
