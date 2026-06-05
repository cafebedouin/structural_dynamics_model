% ============================================================================
% CONSTRAINT STORY: north_sea_maritime_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_north_sea_maritime_sovereignty, []).

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
 *   constraint_id: north_sea_maritime_sovereignty
 *   human_readable: North Sea Maritime Sovereignty and Resource Extraction Rights
 *   domain: geopolitical/maritime_law/economic
 *
 * SUMMARY:
 *   North Sea maritime sovereignty creates a structural tension between
 *   geopolitical claims to exclusive resource access and the reality of
 *   shared ecosystems, overlapping interests, and asymmetric enforcement
 *   capacity. The constraint enables coordination on fishery management and
 *   environmental protection while simultaneously allowing extraction
 *   companies and powerful states to capture resource rents and impose costs
 *   on smaller neighbors, ecological systems, and traditional maritime
 *   communities. The theater ratio (0.48) reflects that Cold War-era naval
 *   doctrine still justifies exclusive zone enforcement, but the strategic
 *   rationale has largely evaporated — the primary function is now resource
 *   protection, not naval power projection. The extractiveness trajectory
 *   shows increasing concentration of benefits and costs over the 30-year
 *   interval as oil/gas production peaked and companies captured larger rents
 *   while bearing externalized environmental costs. The constraint operates
 *   at multiple institutional levels: UNCLOS establishes the coordination
 *   framework (genuine function), national governments enforce boundaries
 *   (mixed coordination and power enforcement), extraction companies profit
 *   under protection (asymmetric extraction), and fishing communities and
 *   marine ecosystems bear uncompensated costs (suppression mechanism).
 *
 * KEY AGENTS:
 *   - Small-scale Fishing Communities: Primary victim (powerless/trapped) — excluded from traditional grounds, bear resource depletion and pollution costs, lack alternative livelihoods
 *   - Oil and Gas Extraction Companies: Primary beneficiary (institutional/arbitrage) — capture resource rents under sovereign protection, externalize environmental costs, can relocate to other jurisdictions
 *   - Coastal State Governments: Dual institutional actor (institutional/constrained) — benefit from resource nationalism and revenue, but constrained by maritime law and neighboring states, bear environmental cleanup liability asymmetrically distributed
 *   - Neighboring Coastal States: Secondary beneficiary and victim (institutional/constrained) — benefit from EEZ exclusivity but face pressure to concede claims or share resources unfairly, constrained by power asymmetries and arbitration mechanisms
 *   - Environmental Systems and Marine Biodiversity: Primary victim (analytical/trapped) — trapped in shared water commons; fish stocks and ecosystems bear pollution and extraction costs with no compensation mechanism
 *   - International Maritime Institutions: Organized agents building exit pathways (organized/constrained) — UNCLOS bodies, environmental agencies, renewable energy frameworks creating alternative governance structures with sunset logic
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees constraint as hybrid coordination-extraction system degrading over time as strategic justification atrophies
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(north_sea_maritime_sovereignty, 0.58).
domain_priors:suppression_score(north_sea_maritime_sovereignty, 0.65).
domain_priors:theater_ratio(north_sea_maritime_sovereignty, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(north_sea_maritime_sovereignty, extractiveness, 0.58).
narrative_ontology:constraint_metric(north_sea_maritime_sovereignty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(north_sea_maritime_sovereignty, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(north_sea_maritime_sovereignty, tangled_rope).
narrative_ontology:human_readable(north_sea_maritime_sovereignty, "North Sea Maritime Sovereignty and Resource Extraction Rights").
narrative_ontology:topic_domain(north_sea_maritime_sovereignty, "geopolitical/maritime_law/economic").

domain_priors:requires_active_enforcement(north_sea_maritime_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(north_sea_maritime_sovereignty, oil_gas_extraction_companies).
narrative_ontology:constraint_beneficiary(north_sea_maritime_sovereignty, flag_state_governments).
narrative_ontology:constraint_victim(north_sea_maritime_sovereignty, neighboring_coastal_states).
narrative_ontology:constraint_victim(north_sea_maritime_sovereignty, maritime_ecological_systems).
narrative_ontology:constraint_victim(north_sea_maritime_sovereignty, fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FISHING COMMUNITIES (SNARE) — Trapped by exclusive economic zone boundaries that restrict access to traditional fishing grounds. Cannot exit without abandoning livelihoods and cultural identity. Bear full cost of maritime sovereignty constraint through resource exclusion, with minimal coordination benefit. No alternative means of subsistence in isolated coastal regions.
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COASTAL STATE REGULATORS (TANGLED ROPE) — Constrained by sovereignty claims that limit their enforcement authority over oil and gas operations in disputed zones. Benefit from resource revenue sharing and coastal protection coordination, but extraction occurs through asymmetric environmental liability distribution — states bear cleanup costs while companies capture profit during operational phase.
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXTRACTION COMPANIES (ROPE) — Primary beneficiary. Experience the constraint as pure coordination: exclusive extraction rights enable long-term capital investment, operational planning, and profit realization. No experience of extraction — they are extracting. Can arbitrage between jurisdictions and regulatory regimes. Net flow of value toward this agent.
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: NEIGHBORING COASTAL STATES (TANGLED ROPE) — Constrained by UNCLOS and maritime boundary agreements that establish sovereign resource rights but also create obligations to consult and coordinate. Benefit from resource nationalism and exclusive economic zones, but extraction occurs through asymmetric enforcement — powerful states enforce their boundaries; weaker neighbors face pressure to concede overlapping claims or share resources below fair value.
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: MARITIME GOVERNANCE COALITION (SCAFFOLD) — Organized actors (UNCLOS institutions, maritime arbitration courts, environmental protection agencies, North Sea Commission) see the constraint as a temporary coordination problem with a sunset. Climate transition and renewable energy mandates are creating alternative frameworks: offshore wind leasing, marine spatial planning, and blue carbon commitments are building parallel governance structures. Suppression is declining as coalition capacity increases. Estimated sunset: 15-25 years as renewable infrastructure replaces fossil fuel extraction.
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COLD WAR STRATEGIC ARCHITECTURE (PITON) — Traditional maritime sovereignty frameworks derive from Cold War naval power projection logic: exclusive zones secure energy access and deny competitors advantage. This justification has atrophied — the strategic payoff of North Sea control is diminished in a post-Cold War, decarbonizing context. But the institutional structures (EEZ enforcement, naval presence, maritime boundary treaties) persist through institutional inertia. Theater ratio high (0.65) because enforcement now centers on preventing 'intrusions' that pose no actual strategic threat, with ritual naval exercises and diplomatic ceremonies maintaining the appearance of contestation where little functional competition exists.
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, North Sea maritime sovereignty exhibits genuine coordination (shared fishing stock management, marine spatial planning, environmental protection) alongside asymmetric extraction (resource rents captured by energy companies, enforcement asymmetries favoring powerful states, ecological costs borne by non-state actors). The constraint is neither pure law-of-the-sea (mountain) nor pure extraction (snare) but a hybrid system where coordination mechanisms coexist with extractive mechanisms. Classification sensitive to time horizon: immediate/biographical perspectives show snare or snare-leaning tangled rope; generational perspectives show tangled rope or scaffold; civilizational perspectives show the architecture itself degrading (piton features emerging).
constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(north_sea_maritime_sovereignty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(north_sea_maritime_sovereignty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(north_sea_maritime_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(north_sea_maritime_sovereignty, TR),
    TR >= 0.70.

:- end_tests(north_sea_maritime_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint enables asymmetric value capture — extraction companies realize profits during 30-40 year operational horizons; resource rents accrue to flag states; costs (pollution, fishery depletion, ecosystem damage) are distributed to communities and natural systems with no compensation mechanism. The base extractiveness increased from 0.42 to 0.58 over the interval as companies optimized production and captured larger margins while regulatory oversight remained constant. Suppression (0.65): Moderate-high. Multiple barriers prevent victims from exiting or reforming the constraint: maritime law creates legal barriers (EEZ boundaries are treaties), economic barriers (fishing communities have no alternatives), military barriers (enforcement through naval presence), and cognitive barriers (sovereignty framing naturalized as immutable international law). Theater ratio (0.48): Moderate. Cold War justifications for exclusive zones persist (strategic denial, naval power projection) but are increasingly disconnected from actual function (resource extraction, not geopolitical competition). The theater is rising as enforcement becomes more about maintaining appearance of contested sovereignty than managing real strategic threat. Naval exercises, diplomatic ceremonies, and boundary patrols serve theatrical functions, but the underlying institutional architecture is still functional for resource protection.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary (rope) and victim (snare) perspectives reveals the constraint's extractive character. The beneficiary's experience of coordination (voluntary resource extraction is economically optimal) depends entirely on their power to escape costs — they can exit jurisdictions, relocate operations, lobby for favorable terms. The victim's experience of extraction (you cannot fish, you cannot contest the boundary, you bear pollution costs) depends entirely on their powerlessness. The same institutional arrangement (UNCLOS EEZ system) is a coordination mechanism to the powerful and an extraction system to the trapped. This perspectival gap is not a measurement error — it is the diagnostic signature of a tangled rope: genuine coordination function embedded in extractive structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position within the extraction flow. Extraction companies with arbitrage options (ability to move operations, access to multiple jurisdictions) experience low d (~0.15) — they are beneficiaries with exit capacity. Coastal states with constrained options (sovereignty claims locked in treaties, power asymmetries limiting renegotiation) experience moderate-high d (~0.50-0.65) — they are dual beneficiaries and victims. Fishing communities with trapped status (no alternative livelihoods, geographic immobility, excluded by law) experience maximum d (0.95) — they are pure victims with zero exit capacity. The sigmoid f(d) amplifies the extracted charge for trapped agents: d=0.95 yields f(d)≈1.42, meaning the effective extraction they experience is 1.4× the base metric. This asymmetry is the core extraction mechanism — the same constraint that beneficiaries experience as minor coordination overhead becomes a snare for trapped agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that North Sea sovereignty is genuinely coordinating multiple functions (fishery management, environmental protection, boundary stability) while simultaneously extracting asymmetric rents. The tangled rope classification prevents false categorization as pure rope (which would ignore the systematic cost-bearing by powerless agents) or pure snare (which would ignore the genuine coordination benefits). The constraint's theater ratio remains moderate (0.48) because the coordination function is real, even though strategic justification has atrophied. Piton features are emerging (Cold War doctrine persisting as degraded ritual) but have not yet dominated — the constraint remains primarily functional in enabling resource extraction, not yet primarily performative. The scaffold perspective identifies the genuine sunset pathway (renewable energy frameworks, marine spatial planning) which will eventually displace the current constraint if implemented equitably. The mandatrophy is not resolved by picking one 'true' classification but by recognizing that the constraint simultaneously instantiates rope (from beneficiary perspective) and snare (from victim perspective), with tangled rope capturing the structure at analytical scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    dispute_resolution_effectiveness,
    'Do maritime arbitration mechanisms (UNCLOS tribunals, ICJ) resolve boundary disputes through genuine negotiation or enforce fait accompli established by military/economic power?',
    'Historical analysis of tribunal outcomes: correlation between ruling and prior military presence, economic capacity, diplomatic leverage; comparison of small-state vs large-state settlement patterns',
    'If genuine negotiation: constraint is coordination-heavy rope/tangled rope. If enforcement of power: constraint is extraction-dominant snare/tangled rope. Classification hinges on whether mechanism enables weak states to secure fair outcomes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dispute_resolution_effectiveness, empirical, 'Whether maritime dispute resolution is genuine coordination or power enforcement').

omega_variable(
    ecological_valuation_asymmetry,
    'Are marine ecosystem costs from oil extraction (pollution, biodiversity loss, fishery collapse) adequately compensated in revenue-sharing or liability frameworks?',
    'Ecological damage assessment vs financial compensation; long-term fishery productivity data; recovery timeline analysis post-decommissioning',
    'If adequately compensated: suppression metric overestimated, tangled rope classification firm. If inadequately compensated: suppression metric underestimated, snare classification for ecological systems and dependent communities.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecological_valuation_asymmetry, empirical, 'Whether ecological costs are fairly compensated').

omega_variable(
    renewable_transition_pathway,
    'Will offshore wind and marine spatial planning frameworks genuinely replace oil/gas governance or become parallel extractive systems with same concentration of benefits and asymmetric costs?',
    'Institutional analysis of renewable energy licensing: distribution of lease allocation, benefit-sharing mechanisms, environmental enforcement; comparison with oil/gas pattern; early implementation data from North Sea wind projects',
    'If genuine replacement: scaffold sunset is real, extractiveness will decline. If new extraction: constraint persists under different label, piton classification will intensify.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_transition_pathway, conceptual, 'Whether renewable transition genuinely replaces extractive governance').

omega_variable(
    maritime_boundary_stability,
    'Are current EEZ boundaries treated as fixed law or contingent arrangements subject to renegotiation as resource values or geopolitical conditions change?',
    'Longitudinal analysis of maritime boundary treaties: frequency of renegotiation, triggers for boundary revision, patterns in consent vs coercion',
    'If truly fixed: constraint has mountain-like stability component. If contingent: constraint vulnerability increases, exit options expand for trapped agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maritime_boundary_stability, empirical, 'Whether maritime boundaries are legally fixed or contingently renegotiable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(north_sea_maritime_sovereignty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nort_tr_t0, north_sea_maritime_sovereignty, theater_ratio, 0, 0.38).
narrative_ontology:measurement(nort_tr_t15, north_sea_maritime_sovereignty, theater_ratio, 15, 0.44).
narrative_ontology:measurement(nort_tr_t30, north_sea_maritime_sovereignty, theater_ratio, 30, 0.48).

% Extraction over time
narrative_ontology:measurement(nort_be_t0, north_sea_maritime_sovereignty, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(nort_be_t15, north_sea_maritime_sovereignty, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(nort_be_t30, north_sea_maritime_sovereignty, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(north_sea_maritime_sovereignty, resource_allocation).
narrative_ontology:affects_constraint(north_sea_maritime_sovereignty, fishery_collapse_north_atlantic).
narrative_ontology:affects_constraint(north_sea_maritime_sovereignty, offshore_renewable_energy_leasing).
narrative_ontology:affects_constraint(north_sea_maritime_sovereignty, arctic_maritime_sovereignty).

% DUAL FORMULATION NOTE:
% North Sea maritime sovereignty decomposes into three structurally distinct constraints with different ε values: (1) resource coordination (fishery management, environmental protection), ε≈0.15 (rope); (2) extraction asymmetry (rent capture by powerful actors), ε≈0.65 (snare); (3) institutional obsolescence (Cold War doctrine persisting through inertia), ε≈0.48 (piton). This story integrates all three as a tangled system. The fishery collapse constraint is downstream — it results from insufficient coordination (treated as exogenous in this story). Renewable energy leasing is parallel — may replace or supplement this constraint. Arctic sovereignty is a sibling — exhibits same structural pattern in geopolitically more contested zone.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(north_sea_maritime_sovereignty, institutional, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
