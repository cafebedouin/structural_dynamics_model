% ============================================================================
% CONSTRAINT STORY: us_foreign_policy_america_first
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_foreign_policy_america_first, []).

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
 *   constraint_id: us_foreign_policy_america_first
 *   human_readable: America First Foreign Policy Doctrine
 *   domain: geopolitical/international_relations
 *
 * SUMMARY:
 *   The 'America First' foreign policy doctrine represents a deliberate shift
 *   from multilateral alliance coordination toward transactional bilateral
 *   relationships, prioritizing short-term US gains over the coordination
 *   function of the post-WWII liberal international order. This constraint
 *   exhibits structural tension between serving a genuine coordination
 *   function (bilateral negotiation, market access) and extracting asymmetric
 *   value from alliance commitment imbalances and hegemonic position. The
 *   doctrine emerged as a response to perceived burden-sharing inequities in
 *   NATO and trade imbalances with China, but its implementation has
 *   fragmented global coordination capacity on climate, pandemic response,
 *   trade rules, and security. The constraint is fundamentally a tangled
 *   rope: it does coordinate — bilateral deals are real agreements with
 *   negotiated terms — but its coordination function is coupled with
 *   asymmetric extraction from allies and the global coordination commons.
 *   The theater ratio (0.64) reflects the performative dimension: summit
 *   diplomacy, trade announcement theater, and nationalist rhetorical
 *   packaging mask the underlying transactional structure. Suppression (0.68)
 *   is significant because allied nations face costliness of defection
 *   (security dependence, geographic constraints) and limited alternative
 *   partnerships, while the global coordination commons (climate agreements,
 *   pandemic response capacity, humanitarian coordination) has no voice and
 *   cannot negotiate.
 *
 * KEY AGENTS:
 *   - US Executive Branch and Nationalist Political Coalition: Primary beneficiary (institutional/arbitrage) — extracts concessions through bilateral leverage, consolidates executive power over foreign policy, captures rents from trade renegotiations
 *   - Domestic Manufacturing Sector: Secondary beneficiary (powerful/mobile) — gains tariff protection and market access concessions, but faces supply chain disruption and retaliation
 *   - Allied Nations (NATO, East Asia Allies): Primary victim (moderate/trapped) — face increased burden-sharing demands, security guarantee uncertainty, and forced renegotiation under threat of abandonment
 *   - Multilateral Alliance System (UN, WTO, NATO, climate agreements): Victim (powerless/trapped) — coordination mechanisms fragment as bilateral logic replaces multilateral governance; cannot negotiate or exit
 *   - Non-US Developed Nations and Emerging Blocs (EU, China, Russia, India): Organized agent (organized/mobile) — experience extraction but can build alternative coordination mechanisms; generate countervailing power through coalition formation
 *   - Post-WWII Liberal International Order: Institutional actor (institutional/constrained) — suffers functional delegitimization but persists through institutional inertia (Piton perspective)
 *   - Global Coordination Commons (climate, pandemic, humanitarian coordination): Victim (powerless/trapped) — abstract collective good bearing the extraction cost; no negotiating capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_foreign_policy_america_first, 0.58).
domain_priors:suppression_score(us_foreign_policy_america_first, 0.68).
domain_priors:theater_ratio(us_foreign_policy_america_first, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_foreign_policy_america_first, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_foreign_policy_america_first, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_foreign_policy_america_first, tangled_rope).
narrative_ontology:human_readable(us_foreign_policy_america_first, "America First Foreign Policy Doctrine").
narrative_ontology:topic_domain(us_foreign_policy_america_first, "geopolitical/international_relations").

domain_priors:requires_active_enforcement(us_foreign_policy_america_first).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, us_executive_branch).
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, domestic_manufacturing_sector).
narrative_ontology:constraint_beneficiary(us_foreign_policy_america_first, nationalist_political_coalition).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, multilateral_alliance_system).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, global_coordination_capacity).
narrative_ontology:constraint_victim(us_foreign_policy_america_first, allied_nations_burden_sharing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL COORDINATION COMMONS (SNARE) — The multilateral alliance system, climate agreements, trade regimes, and humanitarian coordination mechanisms have no exit option and cannot negotiate. They bear the extraction cost as transactional relationships fragment collective capacity. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.97.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALLIED NATIONS (SNARE) — Face increasing burden-sharing demands and security guarantee uncertainty. Exit is constrained by geography and security dependence. Suppressed by threat of abandonment. d≈0.85, f(d)≈1.20, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: DOMESTIC MANUFACTURING SECTOR (TANGLED ROPE) — Benefits from tariff protection and renegotiated trade terms, but also experiences supply chain disruption and retaliation. Mixed coordination (market access) and extraction (tariff rents). d≈0.48, f(d)≈0.60, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 4: US EXECUTIVE BRANCH (ROPE) — Experiences the constraint as coordination: renegotiating terms bilaterally, extracting concessions through transactional leverage, consolidating executive power. Institutional exit via arbitrage (bilateral deals with multiple partners). d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: POST-WWII LIBERAL ORDER (PITON) — The multilateral institutions (UN, WTO, IMF, NATO) persist but their functional legitimacy has eroded. Theater ratio ≈0.64: diplomatic rituals continue (UN votes, trade negotiations) but enforcement mechanism has weakened. The order survives through institutional inertia, not active coordination. d≈0.70, f(d)≈1.10, σ=1.2 → χ≈0.41.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED COMPETITOR NATIONS (TANGLED ROPE) — EU, China, Russia, India respond by building alternative coordination mechanisms (Belt & Road, RCEP, EU strategic autonomy). They experience America First as extraction but can organize countervailing power through coalition-building. Benefits from reduced US hegemonic taxation; costs from bifurcated global system. d≈0.52, f(d)≈0.68, σ=1.1 → χ≈0.43.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From civilizational timescale, international relations 'naturally' reflect state interest maximization; hegemonic states 'naturally' exploit power asymmetries. This perspective risks naturalizing what is actually a contingent policy choice. The structural data (ε=0.58, suppression=0.68) contradicts a mountain classification — America First is a deliberate institutional shift, not an immutable law.
constraint_indexing:constraint_classification(us_foreign_policy_america_first, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_foreign_policy_america_first_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_foreign_policy_america_first, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_foreign_policy_america_first, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_foreign_policy_america_first, TR),
    TR >= 0.70.

:- end_tests(us_foreign_policy_america_first_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval from 0.35 to 0.58. The doctrine began as a renegotiation framework (justified by perceived inequities) but has evolved into systematic extraction of concessions through bilateral leverage, security guarantee uncertainty, and unilateral tariff imposition. The escalation reflects that initial transactional gains encouraged deeper extraction. Suppression (0.68): Significant. Allied nations face costliness of exit (security dependence on US, geographic constraints, economic integration), limiting alternative partnerships. The global coordination commons has zero bargaining power and no exit mechanism. Suppression is not total because some nations are building alternatives (EU strategic autonomy, RCEP, Belt & Road), but this process takes years and faces US countervailing pressure. Theater ratio (0.64): Moderate-high, increasing from 0.42. The doctrine has accumulated performative framing (nationalism, 'winning,' bilateral 'deals') that masks underlying extraction dynamics. Trade announcements and summit diplomacy create theater, while actual enforcement of new terms (tariffs, sanctions, military deployments) carries real cost.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range demonstrates the full extraction-coordination spectrum. The US executive sees pure coordination (Rope) — renegotiating beneficial terms through bilateral leverage. Allied nations and the global coordination commons see pure extraction (Snare) — facing demands with no negotiating capacity. The organized competitor nations (EU, China, India) see tangled rope — experiencing extraction but mobilizing countervailing power. The post-WWII order sees degradation (Piton) — its institutional forms persist but functional legitimacy erodes. The analytical observer risks naturalizing the doctrine as realism (Mountain) — treating state interest maximization as immutable — but the structural data reveals it as a contingent policy shift with computable extraction metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   US Executive Branch: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net institutional beneficiary with low extraction burden. Domestic Manufacturing: Beneficiary + mobile → d≈0.48, f(d)≈0.60. Mixed: gains tariff protection but faces supply chain retaliation; moderate exit capacity. Allied Nations: Victim + trapped → d≈0.85, f(d)≈1.20. High extraction; security dependence and geographic constraints limit exit. Multilateral System: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction; cannot negotiate. Organized Competitors: Victim/Beneficiary (mixed) + mobile → d≈0.52, f(d)≈0.68. Coordinated response capacity moderates extraction burden. Post-WWII Order: Institutional + constrained → d≈0.70, f(d)≈1.10. Piton classification from theater gate, not from high chi. Analytical Observer: analytical → d≈0.72, f(d)≈1.15. Mountain perspective risks naturalizing contingent doctrine.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing coordination function from extraction mechanism. America First IS a coordination mechanism — bilateral negotiation, terms renegotiation, market access agreements — these are real coordinated activity. But it is coupled with asymmetric extraction: the US leverages hegemonic position and allied security dependence to shift surplus distribution unilaterally. The Tangled Rope classification captures this: genuine coordination (bilateral deals, negotiated terms) plus genuine extraction (unilateral leverage, burden-shifting, fragmentation of alternatives). The mandatrophy resolution shows that both aspects are structural, not observational artifacts. Allied nations experience real extraction (higher defense spending, security guarantee uncertainty); the US executive experiences real coordination gains (renegotiated terms, consolidated executive power). The constraint is not 'coordination pretending to be extraction' nor 'extraction pretending to be coordination' — it is genuinely both, which is exactly what Tangled Rope denotes. Theater ratio (0.64) confirms this: there is real performative content (nationalist rhetoric, 'winning' framing, bilateral theater), but it masks actual transactional power shifts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transactional_value_extraction,
    'Is America First extracting net value from bilateral renegotiations or merely reallocating existing terms?',
    'Economic analysis of trade flows, tariff revenue, concession values before/after; comparison of bilateral vs multilateral surplus distribution over 5-10 year period',
    'If extracting: ε should increase to 0.65+. If reallocating: ε stays at 0.58. Classification changes from Tangled Rope to Snare if extraction is confirmed as unilateral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transactional_value_extraction, empirical, 'Whether bilateral renegotiations extract new value or reallocate existing terms').

omega_variable(
    alliance_reconstitution_capacity,
    'Can allied nations reconstitute coordination capacity through alternative mechanisms (EU autonomy, RCEP, AUKUS, Quad) or is the extraction mechanism itself preventing coordination?',
    'Tracking alliance formation speed and institutional depth; measuring whether alternative coalitions reduce or increase global coordination costs; assessing whether bifurcation reduces total surplus',
    'If reconstitution is effective: scaffold perspective becomes more salient (temporary disruption, new norms forming). If reconstitution is blocked: snare classification becomes global consensus, suppression ≥0.75.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alliance_reconstitution_capacity, empirical, 'Whether allied nations can build effective alternative coordination mechanisms').

omega_variable(
    us_exit_cost_visibility,
    'How much of America First''s extraction benefit depends on US agents'' perception of zero exit cost from multilateral commitments, versus actual structural sunk costs?',
    'Comparison of US benefit flows from specific alliance/trade commitments vs historical sunk investment (NATO infrastructure, forward bases, trade dependency); analysis of costs incurred when exiting commitments',
    'If US has high hidden exit costs: US executive branch directionality d should be higher (0.08→0.25+), classification shifts from Rope toward Tangled Rope. If costs are truly minimal: d ≈0.08 confirmed, Rope is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_exit_cost_visibility, empirical, 'Whether US perceives true exit cost or misperceives multilateral commitments as low-cost').

omega_variable(
    hegemonic_maintenance_requirement,
    'Does US hegemony require continued investment in multilateral order maintenance, or can a transactional America First approach sustain hegemonic position indefinitely?',
    'Long-term analysis (10+ years) of US relative power position; measurement of whether bifurcated system increases or decreases US geopolitical leverage; assessment of whether alternative blocs reduce US structural power',
    'If maintenance required: America First is unsustainable extraction (classification eventually converges to Snare for US agents too). If transactional suffices: Rope classification for US executive is sustained.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hegemonic_maintenance_requirement, empirical, 'Whether hegemonic position requires continued multilateral order maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_foreign_policy_america_first, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afirst_tr_t0, us_foreign_policy_america_first, theater_ratio, 0, 0.42).
narrative_ontology:measurement(afirst_tr_t5, us_foreign_policy_america_first, theater_ratio, 5, 0.54).
narrative_ontology:measurement(afirst_tr_t10, us_foreign_policy_america_first, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(afirst_be_t0, us_foreign_policy_america_first, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(afirst_be_t5, us_foreign_policy_america_first, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(afirst_be_t10, us_foreign_policy_america_first, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_foreign_policy_america_first, enforcement_mechanism).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, multilateral_climate_coordination).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, wto_trade_rule_enforcement).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, nato_collective_defense).
narrative_ontology:affects_constraint(us_foreign_policy_america_first, international_pandemic_response).

% DUAL FORMULATION NOTE:
% America First operates as a single constraint across multiple institutional domains (NATO, trade, climate, pandemic response). Each domain experiences the same directionality and extraction pattern but with domain-specific institutional embodiments. Network edges represent contamination: America First's bilateral logic in one domain (trade) influences other domains (NATO burden-sharing demands, climate agreement withdrawal). Downstream constraints in each domain model the fragmentation effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_foreign_policy_america_first, institutional, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
