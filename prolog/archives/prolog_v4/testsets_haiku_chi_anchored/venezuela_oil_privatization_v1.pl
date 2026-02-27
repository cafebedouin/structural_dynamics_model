% ============================================================================
% CONSTRAINT STORY: venezuela_oil_privatization_v1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_venezuela_oil_privatization_v1, []).

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
 *   constraint_id: venezuela_oil_privatization_v1
 *   human_readable: Shadow Privatization of Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Venezuela's oil sector undergoes shadow privatization not through formal
 *   legal mechanisms but through de facto extraction of revenue and
 *   operational control by foreign consortiums, state-connected elites, and
 *   smuggling networks operating outside formal institutional channels. In
 *   response to crippling U.S. and international sanctions (2017-2024),
 *   Venezuelan governance has abandoned open-market oil sales and instead
 *   funneled production through Chinese, Russian, and Iranian middlemen, with
 *   most revenues flowing to regime-connected accounts rather than state
 *   coffers or public benefit. The constraint operates as a tangled hybrid:
 *   foreign actors benefit from cheap sanctioned crude and jurisdictional
 *   arbitrage (coordination function); Venezuelan elites benefit from revenue
 *   capture and patronage consolidation (extraction function); the general
 *   population bears the full cost through hyperinflation, fuel scarcity, and
 *   institutional collapse; and PDVSA's formal institutional role persists
 *   but with operational autonomy stripped by shadow network logistics and
 *   elite oversight. Theater is high (0.68) because the formal sanctions
 *   regime continues producing announcements and designations that have
 *   minimal effect on actual flows, while the real mechanism — shadow
 *   networks and transshipment — operates invisibly. The constraint requires
 *   active enforcement (shadow logistics, elite coordination, military
 *   suppression of dissent) and would collapse if those mechanisms failed,
 *   disqualifying it from mountain status despite appearances of economic
 *   inevitability.
 *
 * KEY AGENTS:
 *   - Venezuelan General Population: Primary victim (powerless/trapped) — bears extraction through hyperinflation, scarcity, state collapse; cannot exit national economy
 *   - PDVSA State Oil Company: Institutional victim (organized/constrained) — nominally continues operations but operational autonomy stripped; maintenance budgets diverted; revenue flows diverted
 *   - Foreign Extraction Consortiums: Primary beneficiary (institutional/arbitrage) — Russian, Chinese, Iranian firms access discounted crude and profit from transshipment arbitrage; can exit shadow networks if less profitable
 *   - State Apparatus Connected Elites: Secondary beneficiary (organized/arbitrage) — capture oil revenues and smuggling profits; have offshore exit options and patronage control
 *   - International Sanctions Regime: Institutional system (institutional/arbitrage) — maintains performative enforcement role while generating the shadow networks that defeat it; persists through institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (elite coordination, smuggling logistics) as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(venezuela_oil_privatization_v1, 0.58).
domain_priors:suppression_score(venezuela_oil_privatization_v1, 0.72).
domain_priors:theater_ratio(venezuela_oil_privatization_v1, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, extractiveness, 0.58).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(venezuela_oil_privatization_v1, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(venezuela_oil_privatization_v1, tangled_rope).
narrative_ontology:human_readable(venezuela_oil_privatization_v1, "Shadow Privatization of Venezuela's Oil Sector").
narrative_ontology:topic_domain(venezuela_oil_privatization_v1, "geopolitical/economic").

domain_priors:requires_active_enforcement(venezuela_oil_privatization_v1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, foreign_extraction_consortiums).
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, state_apparatus_connected_elites).
narrative_ontology:constraint_beneficiary(venezuela_oil_privatization_v1, logistics_smuggling_networks).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, venezuelan_general_population).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, pdvsa_institutional_autonomy).
narrative_ontology:constraint_victim(venezuela_oil_privatization_v1, state_revenue_system).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN GENERAL POPULATION (SNARE) — Trapped by currency controls, import restrictions, and dependence on dwindling fuel subsidies. Cannot exit the constraint or access international markets. Bears extraction through hyperinflation, scarcity, and state collapse while oil revenues evaporate into shadow networks and foreign accounts. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: PDVSA INSTITUTIONAL STRUCTURE (TANGLED ROPE) — Experiences coordination function (oil production logistics, infrastructure maintenance) that benefits legitimate state operations, but also experiences asymmetric extraction through diversion of revenue, maintenance budgets, and operational autonomy to shadow networks and state elites. Constrained by lack of hard currency for repairs and inability to access international markets. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOREIGN EXTRACTION CONSORTIUMS (ROPE) — Benefits from coordination of shadow supply chains (Russian, Chinese, Iranian consortium operations). Experiences the constraint as a profitable coordination mechanism: sanctioned oil moves through legal jurisdictional arbitrage (flag transfers, transshipment, SOCAR intermediation). Low suppression within the consortium network because participating firms are voluntary members. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: STATE APPARATUS CONNECTED ELITES (TANGLED ROPE) — Experiences dual benefit: (a) coordination of internal power consolidation and patronage networks (extraction from state apparatus toward connected individuals), and (b) direct extraction of oil revenues and ancillary smuggling profits. Has arbitrage exit options (offshore accounts, international residency programs, dual citizenship). Suppression of internal dissent within the faction is high (0.72) but targeted rather than universal. d≈0.15, f(d)≈0.01, σ=1.2 → χ≈0.01. Near-zero effective extraction because beneficiary status dominates.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, tangled_rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL SANCTIONS REGIME (PITON) — The formal institutional structure (OFAC designations, sectoral sanctions, maritime restrictions) is performative theater for the actual constraint mechanism (shadow networks, transshipment, sanctions evasion). The regime's functional purpose — reducing Venezuelan oil exports — is largely defeated by the very networks it generates. Theater ratio = 0.68 (sanctions announcements vs actual enforcement effectiveness). The regime persists through institutional inertia; its stated function is largely inert. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.51.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / INTERNATIONAL ECONOMICS VIEW (MOUNTAIN) — From a civilizational analytical perspective, the constraint appears as an immutable consequence of economic physics: a resource-dependent state under sanctions must either (a) accept economic collapse, (b) access black markets, or (c) surrender resource control. The trilemma seems to lack any escape. However, the structural data (ε=0.58, suppression=0.72, theater=0.68, requires_active_enforcement=true) contradicts a true mountain classification — the constraint requires active enforcement (shadow networks, smuggling logistics, elite coordination) and would collapse if those enforcement mechanisms failed. This is a false summit: what appears as an inevitable economic law is actually a contingent institutional arrangement.
constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(venezuela_oil_privatization_v1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(venezuela_oil_privatization_v1, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(venezuela_oil_privatization_v1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(venezuela_oil_privatization_v1, TR),
    TR >= 0.70.

:- end_tests(venezuela_oil_privatization_v1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts approximately 65-75% of remaining oil production value away from Venezuelan public benefit, state revenues, and institutional autonomy. The extraction mechanisms (foreign consortiums taking discounted crude, elites capturing smuggling margins, sanctions evasion logistics) are substantial but not as extreme as pure snare-level (≥0.66) because some coordination function remains (production logistics, refinery operations) and extraction is not uniform across all Venezuelan actors (some elites benefit significantly). The upward trajectory from 0.32 (early sanctions period, 2017) to 0.58 (current) reflects that shadow privatization has deepened as sanctions enforcement has proven ineffective and elite confidence in unaccountability has increased. Suppression (0.72): High. Multiple suppression mechanisms: (a) state apparatus monopoly on petroleum commerce and foreign exchange; (b) capital controls blocking alternative economic activity; (c) military control of key infrastructure and supply routes; (d) informational suppression — state media silence on revenue diversion; (e) exit suppression — millions have emigrated but remaining population has no legal path to external markets or alternative governance. Suppression is high but not maximal (0.72 not 0.90) because some shadow networks operate with voluntary participation by foreign actors and some internal dissent is tolerated if quiet. Theater (0.68): High. Formal institutions persist (PDVSA board meetings, Ministry of Petroleum press releases, international sanction announcements) but these are largely performative. Real mechanism operates through shadow logistics and elite networks operating invisibly. Theater has risen from 0.42 (2017: sanctions newly imposed, formal institutions still functional) to 0.68 (current: formal institutions are hollow shells; real power in shadow networks). Theater would rise further if complete institutional hollowing occurred, but currently some formal activity continues.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests radically differently across structural positions. The Venezuelan general population sees pure snare (trapped, extraction-only, no coordination benefit). PDVSA sees tangled rope (some coordination function in production logistics; extraction of revenue and autonomy). Foreign consortiums see rope (profitable coordination of shadow supply chains; low suppression because participation is voluntary). State elites see near-rope (profit concentration, low personal extraction, arbitrage exit options). The sanctions regime sees itself as mountain (inevitable economic consequence of sanctions; no escape) but engine analysis reveals false summit (requires active enforcement; would collapse if enforcement mechanisms failed). The civilizational observer risks false summit thinking (naturalizing contingent institutional arrangements as law). This perspectival spread (snare → rope → mountain range) is the diagnostic indicator that the constraint is contingent institutional arrangement rather than natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Venezuelan General Population: Victim + trapped → d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Maximum extraction. Cannot exit or migrate; dependent on state currency and import system that collapses due to revenue diversion. Foreign Extraction Consortiums: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary. Can exit shadow networks if terms worsen; participate voluntarily. State Apparatus Connected Elites: Beneficiary + arbitrage → d≈0.15, f(d)≈0.01, σ=1.2 → χ≈0.01. Near-zero effective extraction; strong beneficiary position. PDVSA Institutional: Mixed victim/beneficiary + constrained → d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.59. High extraction due to revenue/autonomy diversion; constrained exit (formally part of state apparatus). International Sanctions Regime: Observer + arbitrage → d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.51. Piton classification (theater gate) despite moderate directionality; classification comes from performative institutional structure rather than extraction dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved through inter-institutional perspective decomposition. The constraint is simultaneously Tangled Rope (at the state institutional level: coordination + extraction), Snare (at the population level: pure extraction), and Rope (at the foreign consortium level: pure coordination). These are not errors in classification but faithful reflections of structural positions. The apparent paradox that 'the same constraint is both rope and snare' resolves when we recognize that 'the constraint' is not singular but relational: what is rope-like for foreign actors (coordinated entry to shadow markets with profit opportunities) is snare-like for the population (trapped in collapsing peso with no alternative). The analytical observer's temptation to call this a mountain (inevitable consequence of economic physics and sanctions) is a false summit — the structural mechanisms (elite coordination, military enforcement, smuggling logistics) would collapse if key actors defected or enforcement mechanisms failed. Therefore, the constraint is correctly classified as Tangled Rope at the system level (coordination + enforcement + asymmetric extraction), while perspectives from local positions (powerless/trapped) read it as snare, and perspectives from beneficiary positions read it as rope. No single type describes the constraint from all positions; the presheaf over positions is the correct representation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sanctions_effectiveness_threshold,
    'At what volume of shadow transactions does sanctions enforcement become genuinely ineffective rather than merely diminished?',
    'Comparative analysis of sanctions regimes (Iranian, North Korean, Russian) and their shadow transaction volumes; correlation between enforcement intensity and market diversion success',
    'If threshold is low (< 30% diversion): sanctions maintain meaningful leverage; constraint is primarily extractive within Venezuela. If threshold is high (> 70% diversion): sanctions have created a self-defeating extraction mechanism; the international regime is piton-tier performative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sanctions_effectiveness_threshold, empirical, 'Sanctions effectiveness threshold').

omega_variable(
    elite_factional_unity,
    'Do Venezuelan state elites maintain sufficient factional unity to prevent internal defection and alternative governance coalition formation?',
    'Intelligence analysis of elite defections, inter-faction conflicts, military loyalty distributions, and international recognition dynamics',
    'If unity is maintained: tangled_rope classification is stable; extraction continues through consolidated networks. If unity fractures: the constraint collapses into multiple competing snares and tangled_ropes; regime sustainability becomes dependent on armed suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_factional_unity, empirical, 'Cohesion of state-apparatus elite networks').

omega_variable(
    oil_production_ceiling,
    'Does Venezuela''s current oil production floor (≤800k bbl/day) represent irreversible infrastructure degradation or maintenance deferred by sanctions?',
    'Technical assessment of PDVSA refinery capacity, crude quality requirements, infrastructure lifespan, capital requirements for restoration, and comparison with pre-sanctions baseline',
    'If irreversible: constraint becomes a mountain (physical infrastructure limit); revenue extraction becomes moot as production approaches zero. If reversible: constraint is tangled_rope with actionable policy alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(oil_production_ceiling, empirical, 'Whether oil production collapse is reversible or irreversible').

omega_variable(
    alternative_supplier_substitution,
    'Can Venezuela''s primary shadow customers (Russia, Iran, China) be substituted by alternative suppliers without losing market dominance or accepting price reductions?',
    'Global crude capacity analysis, cost differentials, logistics network dependencies, and strategic alliance stability assessments',
    'If substitution is easy: Venezuela''s bargaining position in shadow networks is weak; extraction mechanisms are externally controlled. If substitution is difficult: Venezuela retains some leverage; extracted value may be somewhat higher than current assessments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_supplier_substitution, empirical, 'Substitutability of Venezuela as shadow oil supplier').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(venezuela_oil_privatization_v1, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(venoil_tr_t0, venezuela_oil_privatization_v1, theater_ratio, 0, 0.42).
narrative_ontology:measurement(venoil_tr_t5, venezuela_oil_privatization_v1, theater_ratio, 5, 0.55).
narrative_ontology:measurement(venoil_tr_t10, venezuela_oil_privatization_v1, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(venoil_be_t0, venezuela_oil_privatization_v1, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(venoil_be_t5, venezuela_oil_privatization_v1, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(venoil_be_t10, venezuela_oil_privatization_v1, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(venezuela_oil_privatization_v1, enforcement_mechanism).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, iranian_oil_sanctions_architecture).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, russian_energy_leverage).
narrative_ontology:affects_constraint(venezuela_oil_privatization_v1, chinese_petro_colonialism_latin_america).

% DUAL FORMULATION NOTE:
% Shadow privatization of Venezuela's oil sector is downstream of the international sanctions regime but represents a distinct structural constraint. The upstream sanctions architecture (OFAC designations, sectoral restrictions) has its own ε and theater values; shadow privatization has ε=0.58 reflecting the actual extraction mechanisms that have emerged in response to sanctions. The two constraints should be modeled separately with network link capturing causal dependency: sanctions architecture → shadow privatization emergence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(venezuela_oil_privatization_v1, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
