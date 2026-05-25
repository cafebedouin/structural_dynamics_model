% ============================================================================
% CONSTRAINT STORY: cuba_mandatrophic_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cuba_mandatrophic_collapse, []).

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
 *   constraint_id: cuba_mandatrophic_collapse
 *   human_readable: Cuban Mandatrophy (The GAESA-Infrastructure Divergence)
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   Cuban mandatrophy describes the terminal wasting of civilian state
 *   margins (energy infrastructure, agriculture, public health) caused by
 *   rigid institutional prioritization of the Military-Tourism Mandate —
 *   specifically GAESA's control of hard-currency revenue streams and
 *   strategic resource allocation. The constraint exhibits the paradox of
 *   institutional extraction: the mechanism that was designed to ensure
 *   regime survival (military economic autonomy) produces structural
 *   degradation that ultimately threatens the system it protects. The
 *   mandatrophy is uniquely terminal because the constraint exhibits both
 *   high extractiveness (0.78) and high theater (0.68), indicating both real
 *   resource diversion AND increasing performative legitimacy maintenance.
 *   The regime must simultaneously extract from civilian infrastructure AND
 *   perform the role of a state managing that infrastructure — the
 *   contradiction between these roles drives the classification cascade
 *   across six distinct perspectives.
 *
 * KEY AGENTS:
 *   - GAESA Military Leadership: Primary beneficiary (institutional/arbitrage) — captures hard-currency tourism revenue, maintains economic autonomy from civilian state collapse, controls strategic resource allocation
 *   - Food-Insecure Population: Primary victim (powerless/trapped) — rationed to subsistence levels, no exit options, no advocacy mechanism, bears extraction with zero agency
 *   - Agricultural Sector: Secondary victim (powerless/trapped) — fuel and fertilizer diverted to GAESA operations, investment starved, productivity collapses, trapped by state control and geography
 *   - Public Health System: Secondary victim (powerless/trapped) — chronic shortages, power outages, medication diversion, trapped by profession and state sanctions against exit
 *   - International Humanitarian Agencies: Tertiary actor (organized/constrained) — provide mitigation but constrained by regime gatekeeping, see mandate as mixed coordination-extraction problem
 *   - State Rationing Apparatus: Institutional theater (institutional/arbitrage) — libreta distribution system persists performatively despite functional collapse, maintains equity narratives while actual allocation bypasses formal system
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing mandate as regime-survival necessity, but structural data reveals contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cuba_mandatrophic_collapse, 0.78).
domain_priors:suppression_score(cuba_mandatrophic_collapse, 0.82).
domain_priors:theater_ratio(cuba_mandatrophic_collapse, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, extractiveness, 0.78).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cuba_mandatrophic_collapse, snare).
narrative_ontology:human_readable(cuba_mandatrophic_collapse, "Cuban Mandatrophy (The GAESA-Infrastructure Divergence)").
narrative_ontology:topic_domain(cuba_mandatrophic_collapse, "political/economic/technological").

domain_priors:requires_active_enforcement(cuba_mandatrophic_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cuba_mandatrophic_collapse, military_gaesa_leadership).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, civilian_infrastructure).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, agricultural_sector).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, public_health_system).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, food_insecure_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOOD-INSECURE POPULATION (SNARE) — Trapped within Cuba's borders, without exit options. Bears the full cost of resource diversion: rationed calories, degraded electricity for refrigeration, loss of agricultural productivity. Maximum experienced extraction with zero degrees of freedom. No advocacy, no alternative supply chains available.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: AGRICULTURAL SECTOR (SNARE) — Constrained by fuel rationing, fertilizer diversion to GAESA operations, and lack of investment. Rural infrastructure deteriorates while tourism infrastructure receives priority. Trapped by geography and state control; no exit from the mandate hierarchy.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLIC HEALTH SYSTEM WORKERS (SNARE) — Trapped in a collapsing system: chronic medication shortages, power outages in hospitals, resource diversion to military medical facilities serving GAESA. Cannot exit profession without risking state sanctions. Experience pure extraction without coordination benefit.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: GAESA MILITARY LEADERSHIP (ROPE) — Experiences the mandate as pure coordination: the prioritization mechanism solves the institutional problem of maintaining regime security through economic autonomy. GAESA has arbitrage options (hard-currency tourism revenue, import negotiation leverage). Net beneficiary with operational agency and visible benefit.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL HUMANITARIAN AGENCIES (TANGLED ROPE) — See the mandate as a mixed extraction-coordination problem: coordinating emergency food aid and medical supplies requires negotiation with GAESA (constrained exit), yet also functions as the primary mechanism enabling partial mitigation of the mandate's worst effects. Both constrained by regime gatekeeping and enabled by maintaining a relationship with GAESA institutions.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: STATE RATIONING APPARATUS (PITON) — The official distribution system (libreta) persists as performative infrastructure: it no longer functions as a coordination mechanism (supplies are insufficient to meet stated rations) but continues theatrically, maintaining legitimacy narratives about equity. Theater ratio is high; actual resource allocation bypasses the formal system through informal markets. The apparatus is maintained through institutional inertia despite functional collapse.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / REGIME STABILITY THESIS (MOUNTAIN) — This perspective risks naturalizing the mandate as an immutable law of authoritarian regime survival: military-economic consolidation is inherent to maintaining power against external pressure. However, the structural data contradicts the mountain classification. The mandate is contingent on specific historical conditions (US embargo, Cold War legacy, post-Soviet transition), not natural law. This is a false summit: the engine will identify it as naturalization of a political choice, not a physical necessity.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuba_mandatrophic_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cuba_mandatrophic_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cuba_mandatrophic_collapse, TR),
    TR >= 0.70.

:- end_tests(cuba_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The measurement trajectory shows acceleration from 0.45 (1990s post-Soviet transition) to 0.78 (present). The mandate extracts from all civilian margins simultaneously: fuel rationing reduces agricultural productivity and electricity access; hard-currency requirements divert agricultural outputs to export; military medical facilities draw specialized personnel and medications from civilian health system. No single margin can be sacrificed without cascading failures in others, creating total extraction without differentiation or escape routes. Suppression (0.82): Extremely high. Exit from Cuba is criminalized or prohibitively expensive; internal criticism of the mandate is state suppression; alternative food sources are illegal (hoarding, private markets); information about regime alternatives is restricted; resources for organizing civilian resistance are absent (no fuel for assembly, no communications infrastructure, food insecurity reduces organizing capacity). The suppression is multifaceted and mutually reinforcing. Theater ratio (0.68): Moderately high. The regime maintains narratives of equitable distribution (libreta system persists visibly despite non-functionality), international socialist solidarity (humanitarian aid relationships), and economic sovereignty (GAESA's autonomy as rational defense strategy). Meanwhile, actual resource allocation occurs through informal markets and elite access. The theater is not maximal (0.85+) because the constraint's effects are visibly catastrophic — the performative legitimacy maintenance is increasingly transparent to the population. The measurement trajectory shows theater rising from 0.42 to 0.68 as the regime's narrative sophistication increases despite material degradation.
 *
 * PERSPECTIVAL GAP:
 *   The mandatrophic divergence is measured in the gap between GAESA's classification (Rope: coordination) and the food-insecure population's classification (Snare: pure extraction). In a functional state, both perspectives would produce the same type (likely Tangled Rope if there were genuine tradeoffs between security and welfare). Instead, they produce opposite classifications: one agent experiences beneficial coordination, the other experiences maximum extraction. This perspectival inversion IS the mandatrophy. It indicates that the constraint no longer functions as a comprehensible institutional arrangement (where trade-offs are transparent and roughly equitable). Instead, it has become a pure extraction mechanism disguised by performative legitimacy. The theater ratio rise (0.42 to 0.68) quantifies this divergence: as the constraint becomes more extractive, the regime must invest more in theatrical maintenance of legitimacy to prevent the divergence from becoming visible enough to trigger organized resistance.
 *
 * DIRECTIONALITY LOGIC:
 *   GAESA leadership experiences low directionality (d ≈ 0.10) because they are institutional beneficiaries with arbitrage options: hard-currency revenue, negotiating leverage, resource priority. This produces negative effective extraction chi via the sigmoid — they benefit from the mandate. The food-insecure population experiences high directionality (d ≈ 0.92) because they are powerless, trapped, with no exit and no benefit: maximum experienced extraction. Agricultural workers experience similar high directionality (d ≈ 0.88) — trapped by state control, bearing resource extraction, receiving no coordination benefit. Public health workers experience moderate-high directionality (d ≈ 0.80) — some are regime-aligned (military medical personnel benefit), but most are trapped in a collapsing system with negative career prospects if they attempt exit. International humanitarian agencies experience medium directionality (d ≈ 0.52) — they are both constrained by regime gatekeeping (pushes d upward) and enabled by providing essential services (pulls d downward). The analytical observer at civilizational scope risks d ≈ 0.30 (false beneficiary status) if they naturalize the mandate as regime-survival necessity, but structural analysis corrects this to d ≈ 0.65 (observer exposed to mandate's real extraction mechanisms through research access).
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] Reviewed 2026-03-01. Override: false_natural_law.
 *   MANDATROPHY RESOLUTION (ε = 0.78): The constraint classifies as Snare from the analytical perspective (powerless/trapped/biographical/national axis dominates), and the mandatrophic divergence between GAESA's Rope experience and victims' Snare experience reveals the terminal state of the institutional arrangement. The mandate began as Tangled Rope coordination (post-Soviet transition required resource concentration for regime survival, WITH some benefit to state legitimacy and collective security narrative). It has degraded through three phases: (1) Tangled Rope (1990s-2000s): genuine tradeoff between security and welfare, both perspectives recognized coordination function alongside extraction; (2) Piton (2000s-2015): theater increasing faster than functional benefit, performers (GAESA) recognizing the process as degraded but maintaining it through inertia; (3) Snare (2015-present): coordination function entirely dissolved, pure extraction mechanism masked by performative theater. The mandatrophic resolution is that the constraint has become cognitively unsustainable: it cannot simultaneously be a 'necessary security measure' (Rope legitimation narrative) and a 'terminal civilian degradation' (Snare structural reality). One of these must collapse — either the regime's legitimacy narrative (triggering institutional reform or collapse) or the civilian population's capacity to survive under extraction (triggering migration, social breakdown, or organized resistance). The high theater ratio indicates the regime is aware of the divergence and increasing narrative investment to bridge it — but there are no new narratives available that reconcile 0.78 extractiveness with regime-survival justification. The omega variables are attempts to find a coherent framing that resolves the mandatrophy (regime-survival necessity vs. contingent choice; embargo-driven vs. choice-driven; agricultural collapse causation). Until one of these resolves in the regime's favor, mandatrophy persists unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_survival_threshold,
    'Is the military-tourism mandate a structural requirement for regime survival, or a contingent institutional choice that could be relaxed without existential threat?',
    'Comparative analysis of non-aligned authoritarian regimes with similar external pressure but different economic prioritization models; historical counterfactual analysis of pre-mandate resource allocation regimes in Cuba',
    'If structural necessity: mandate appears mountain-like (immutable). If contingent choice: mandate is pure snare (extractive power without regime-survival justification). Classification cascade affects all downstream perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_survival_threshold, conceptual, 'Whether the mandate is regime-survival-structural or contingent political choice').

omega_variable(
    embargo_counterfactual,
    'How much of the mandate''s severity is driven by the US embargo''s resource scarcity, vs. how much is driven by regime choice within available constraints?',
    'Economic modeling of post-embargo resource allocation; comparison with similar-GDP nations without embargo; analysis of pre-embargo mandate intensity (1950s-1960s data)',
    'If embargo-driven: much of the extraction appears as coordination response to external constraint (Rope reframes Snare). If regime-choice-driven: the mandate is pure extraction (Snare confirmed). Affects beneficiary/victim directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embargo_counterfactual, empirical, 'How much of mandate severity is embargo-driven vs. regime-choice-driven').

omega_variable(
    agricultural_collapse_causation,
    'Is agricultural collapse primarily caused by GAESA resource diversion, or primarily by structural ecological limits (soil degradation, climate variability, lost Soviet inputs)?',
    'Agronomic analysis of soil health trends; comparison of fuel/fertilizer diversion to GAESA vs. baseline agricultural requirements; climate impact modeling; counterfactual yield modeling with historical input levels',
    'If diversion-driven: mandate appears as pure snare on agricultural sector (high extractiveness). If ecology-driven: apparent extraction is actually coordination response to natural constraint (reduces snare classification intensity). Affects victims list and suppression score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_collapse_causation, empirical, 'Agricultural collapse causation: mandate diversion vs. ecological limits').

omega_variable(
    gaesa_autonomy_dependency,
    'Does GAESA''s economic autonomy actually insulate the military from civilian-sector collapse, or does it create hidden dependencies on the civilian infrastructure it appears to abandon?',
    'Network analysis of GAESA supply chains; mapping of electricity, water, fuel supply routes to GAESA facilities; analysis of food price volatility impact on military purchasing power; tracking of brain drain from military due to family-sector collapse',
    'If autonomy is real: GAESA truly experiences rope (pure benefit). If hidden dependency: GAESA experiences tangled rope (extraction comes with coupling to failing civilian systems). Affects GAESA perspective directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gaesa_autonomy_dependency, empirical, 'Whether GAESA''s autonomy is real or creates hidden dependencies on civilian collapse').

omega_variable(
    humanitarian_aid_trap,
    'Does international humanitarian aid enable regime survival (reducing pressure for mandate reform), or does it relieve civilian suffering without addressing the mandate''s structural extraction?',
    'Longitudinal analysis of aid flows vs. regime pressure; modeling of aid impact on food security with and without mandate prioritization; comparative analysis of aid-receiving authoritarian regimes that did vs. did not reform extraction mechanisms',
    'If enabler: international aid legitimizes the snare (tangled rope perspective is correct). If relief without reform: aid is pure mitigation with no structural impact (snare intensity unchanged). Affects humanitarian agency perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_aid_trap, preference, 'Whether humanitarian aid enables regime survival or provides relief without structural reform').

omega_variable(
    reform_feasibility,
    'Could the military-tourism mandate be reformed to a more balanced prioritization without military destabilization, or does it represent a locked-in equilibrium where any reduction in GAESA resource capture triggers institutional collapse?',
    'Institutional economics analysis of GAESA''s sunk costs and lock-in mechanisms; comparative case study of authoritarian regimes that reformed military-economic consolidation; simulation of resource reallocation scenarios with stress-testing of military institutional stability',
    'If reform is feasible: snare classification is correct (extractive choice, not structural necessity). If locked-in: snare appears mountain-like (immutable institutional equilibrium). Affects mandatrophy resolution path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_feasibility, conceptual, 'Whether the mandate is locked-in or could be reformed without institutional collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cuba_mandatrophic_collapse, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cuba_tr_t0, cuba_mandatrophic_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cuba_tr_t15, cuba_mandatrophic_collapse, theater_ratio, 15, 0.55).
narrative_ontology:measurement(cuba_tr_t30, cuba_mandatrophic_collapse, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(cuba_be_t0, cuba_mandatrophic_collapse, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(cuba_be_t15, cuba_mandatrophic_collapse, base_extractiveness, 15, 0.62).
narrative_ontology:measurement(cuba_be_t30, cuba_mandatrophic_collapse, base_extractiveness, 30, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cuba_mandatrophic_collapse, resource_allocation).
narrative_ontology:affects_constraint(cuba_mandatrophic_collapse, soviet_embargo_regime).
narrative_ontology:affects_constraint(cuba_mandatrophic_collapse, cuban_migration_pressure).
narrative_ontology:affects_constraint(cuba_mandatrophic_collapse, gaesa_rentier_consolidation).

% DUAL FORMULATION NOTE:
% The Cuban mandate is downstream of the US embargo constraint (upstream: embargo creates resource scarcity forcing prioritization). Mandatrophy is distinct from the embargo itself because it describes not the scarcity but the institutional response to scarcity — a choice about how to distribute the scarcity. This choice has become terminal. The mandate affects downstream constraints: migration pressure (population seeks exit from mandatrophy), GAESA consolidation (military institution self-reinforces through control of resources), and regime legitimacy (regime must perform increasingly sophisticated theater to maintain mandate's legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
