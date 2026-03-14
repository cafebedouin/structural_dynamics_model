% ============================================================================
% CONSTRAINT STORY: syrian_state_capacity_recovery
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_syrian_state_capacity_recovery, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: syrian_state_capacity_recovery
 *   human_readable: Syrian State Capacity Recovery Through International Reconstruction
 *   domain: geopolitical/state_consolidation/post-conflict
 *
 * SUMMARY:
 *   Syrian state capacity recovery after the 2011-2022 civil war represents a
 *   complex post-conflict consolidation process where regime reconstruction,
 *   international engagement, and civilian welfare operate under structural
 *   tension. The constraint exhibits a hybrid coordination-extraction
 *   structure (tangled rope at multiple institutional levels) with
 *   significant variation across perspectives. The regime experiences
 *   reconstruction as necessary coordination of state apparatus and security
 *   integration; international backers (Russia, Iran) experience it as
 *   geopolitical positioning and resource access coordination; but civilian
 *   populations and opposition groups experience it as suppression and
 *   extraction under the banner of stability and recovery. The constraint's
 *   theater ratio (0.68) reflects extensive performative international
 *   engagement — UN mechanisms, reconstruction conferences, donor boards —
 *   that coexists with minimal accountability enforcement and continued
 *   security force abuses. The extractiveness trajectory (declining from 0.72
 *   to 0.58 over six years) suggests that as reconstruction embeds
 *   institutional capacity, the overt extraction mechanisms may moderate
 *   slightly (less dramatic consolidation violence required), but this
 *   moderating extractiveness does not indicate coordination breakdown — it
 *   reflects ossification of regime control and entrenchment of asymmetric
 *   outcomes. The key analytical question is whether the reconstruction
 *   constraint enables a future transition to more equitable governance
 *   structures (scaffold sunset logic) or entrenches regime consolidation
 *   permanently (snare with structural suppression).
 *
 * KEY AGENTS:
 *   - Damascus Regime Authority: Primary beneficiary (institutional/arbitrage) — consolidates state monopoly on violence and administrative control; captures reconstruction resources through patronage networks
 *   - Syrian Civilian Population: Primary victim (powerless/trapped) — bears costs of reconstruction (property confiscation, conscription, displacement control) with minimal exit capacity or benefit
 *   - Opposition and Displaced Groups: Secondary victim (organized/constrained) — face military defeat and international abandonment; constrained from organizing alternative governance by security suppression and exile fragmentation
 *   - Russian Federation: Secondary beneficiary (powerful/constrained) — coordinates military presence, energy exploration, geopolitical positioning through Syrian reconstruction
 *   - Iranian Regional Network: Secondary beneficiary (powerful/constrained) — coordinates regional axis and security depth through Syria at cost to Iraqi stability and Lebanese economy
 *   - International Reconstruction Institutions: Institutional actor (institutional/arbitrage) — maintains performative engagement through UN processes, donor coordination, accountability mechanisms with minimal enforcement
 *   - Civil Society and Human Rights Organizations: Organized actor (organized/mobile) — documents abuses, supports displaced populations, builds alternative institutional capacity with sunset logic toward transition
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing regime consolidation as inevitable law of post-conflict transition rather than contingent geopolitical choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(syrian_state_capacity_recovery, 0.58).
domain_priors:suppression_score(syrian_state_capacity_recovery, 0.72).
domain_priors:theater_ratio(syrian_state_capacity_recovery, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(syrian_state_capacity_recovery, extractiveness, 0.58).
narrative_ontology:constraint_metric(syrian_state_capacity_recovery, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(syrian_state_capacity_recovery, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(syrian_state_capacity_recovery, tangled_rope).
narrative_ontology:human_readable(syrian_state_capacity_recovery, "Syrian State Capacity Recovery Through International Reconstruction").
narrative_ontology:topic_domain(syrian_state_capacity_recovery, "geopolitical/state_consolidation/post-conflict").

domain_priors:requires_active_enforcement(syrian_state_capacity_recovery).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(syrian_state_capacity_recovery, damascus_regime).
narrative_ontology:constraint_beneficiary(syrian_state_capacity_recovery, russian_interests).
narrative_ontology:constraint_beneficiary(syrian_state_capacity_recovery, iranian_interests).
narrative_ontology:constraint_victim(syrian_state_capacity_recovery, civilian_population).
narrative_ontology:constraint_victim(syrian_state_capacity_recovery, opposition_groups).
narrative_ontology:constraint_victim(syrian_state_capacity_recovery, regional_stability).
narrative_ontology:constraint_victim(syrian_state_capacity_recovery, international_law_norms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SYRIAN CIVILIAN POPULATION (SNARE) — Trapped within the state consolidation process with no exit capacity. Reconstruction occurs under regime authority with security forces controlling movement and resource allocation. High suppression (checkpoints, arrest risk, economic dependency on state jobs). Minimal coordination benefit — reconstruction funds flow through patronage networks rather than equitable delivery. Maximum extraction experienced: civilians bear costs (forced displacement, property confiscation, labor conscription) without meaningful choice or benefit.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OPPOSITION AND DISPLACED GROUPS (TANGLED ROPE) — Organized at regional level but constrained by military disadvantage and international abandonment. Genuine coordination need exists: displaced populations require coordinated return mechanisms, property disputes require judicial resolution, opposition groups require some political framework for participation. But the reconstruction coordination mechanism enforces regime consolidation at asymmetric cost — opposition exclusion from decision-making, property restitution blocked, security guarantees absent. Mixed extraction: some coordination benefit (stabilization enables return) alongside asymmetric extraction (regime captures political outcome).
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DAMASCUS REGIME AUTHORITY (ROPE) — Institutional actor with arbitrage option: can trade reconstruction participation with different international backers (Russia, Iran, Gulf states) to maximize consolidation advantage. Experiences reconstruction as pure coordination mechanism — state rebuilding requires institutional coordination of security forces, administrative apparatus, service delivery. The regime's perspective sees no extraction: the constraint solves the fundamental coordination problem of reasserting state authority. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RUSSIAN FEDERATION INTERESTS (TANGLED ROPE) — Powerful actor constrained by Syria's structural weakness and international pressure. Genuine coordination need: military base security, economic leverage in Eastern Mediterranean, counterweight to US-aligned regional powers. Reconstruction mechanism coordinates these interests by channeling through Syrian state capacity. But extraction is asymmetric: Russian interests extract geopolitical positioning and resource access (phosphate mining deals, energy exploration rights) at cost to Syrian sovereignty and civilian welfare. Measured as constrained rather than mobile because exiting Syria risks losing Levantine foothold.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: IRANIAN REGIONAL NETWORK (TANGLED ROPE) — Powerful actor constrained by sanctions and isolation. Reconstruction mechanism coordinates Iran's regional axis (Iraq, Lebanon, Syria) and provides counterbalance to Gulf states and Israel. Genuine coordination element: integrated defense systems, economic corridors, shared security architecture. But extraction is asymmetric: Iranian interests extract security depth and regional influence at cost to Syrian independence — Lebanese economy suffers, Iraqi stability deteriorates, Syrian civilians bear costs of proxy conflicts. Constrained because Iran's regional position depends on Syria's viability.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: INTERNATIONAL RECONSTRUCTION FRAMEWORKS (PITON) — UN-mandated reconstruction architecture, World Bank coordination, IMF structural adjustment programs, Geneva process diplomatic theater. Theater ratio 0.68: formal mechanisms persist (reconstruction conferences, donor coordination boards, accountability processes) but lack functional enforcement. Sanctions compliance theater, accountability mechanisms with no prosecutorial follow-through, reconstruction plans with minimal implementation. The institutional apparatus persists through legitimacy claims and diplomatic necessity, not functional capacity. International community maintains performative engagement while extractive regime consolidation proceeds unchecked.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: HUMAN RIGHTS & CIVIL SOCIETY (SCAFFOLD) — Organized actors with exit options (operate regionally, document remotely, shift focus). See reconstruction as temporary phase with sunset logic: as international pressure builds, accountability mechanisms mature, and generational leadership changes occur, extractive regime consolidation becomes unsustainable. Building alternative institutional capacity: transitional justice documentation, displaced community networks, civil society infrastructure. Theater ratio lower than regime perspective: NGO mechanisms emphasize functional capacity-building rather than performative legitimacy. Suppression modulates through cycles of crackdown and international pressure.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, state recovery after civil war requires some degree of regime consolidation and security force integration — this is universal across post-conflict transitions. The constraint appears immutable: you cannot rebuild state capacity without concentrating authority; concentration of authority involves suppression and extraction; exit options collapse during the consolidation phase. However, the base_properties metrics (extractiveness 0.58, suppression 0.72) suggest this is NOT a natural law but a contingent institutional choice: many post-conflict states have achieved recovery with lower extraction (Rwanda, Mozambique, El Salvador incorporated opposition into transition mechanisms; extractiveness measured at 0.35-0.42). The mountain classification is a false summit — naturalizes what is a specific geopolitical choice.
constraint_indexing:constraint_classification(syrian_state_capacity_recovery, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(syrian_state_capacity_recovery_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(syrian_state_capacity_recovery, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(syrian_state_capacity_recovery, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(syrian_state_capacity_recovery, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(syrian_state_capacity_recovery, TR),
    TR >= 0.70.

:- end_tests(syrian_state_capacity_recovery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regime extracts state monopoly, security control, and patronage advantage from the reconstruction process. International backers extract geopolitical positioning and resource access. But the extraction is not maximal (0.72 would be closer to pure snare) because some coordination functions are genuine: displaced populations do need integrated return mechanisms, security apparatus does require institutional consolidation, and economic recovery does require some level of state capacity. The declining trajectory (0.72→0.58) reflects that overt consolidation violence decreases as institutions ossify — less dramatic coercion required when the apparatus is established. Suppression (0.72): High and persistent. Checkpoints, arrest risk for opposition activity, property control through security apparatus, movement restrictions on displaced populations, economic dependency on state employment. Suppression does not decline in measurements because it is structural to the consolidation process — it may shift form (checkpoints become routine rather than emergency) but does not decrease. Theater ratio (0.68): High and increasing. Performative reconstruction mechanisms include UN coordination bodies with no enforcement capacity, donor conferences producing plans with minimal implementation, transitional justice processes that document but don't prosecute, accountability mechanisms that appear but lack follow-through. Theater increases because as regime consolidation succeeds, international engagement becomes increasingly symbolic — sustained to maintain legitimacy while functional enforcement collapses.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival variance: the regime sees coordination (rope), civilian populations see extraction (snare), international institutions perform engagement while enforcement collapses (piton), organized opposition see mixed coordination with asymmetric cost (tangled rope at constrained level), powerful external actors see geopolitical positioning coordination (tangled rope at powerful level), civil society sees temporary phase with transition possibility (scaffold), and the analytical observer risks seeing immutable post-conflict law (mountain). The gap between the regime's coordination experience and the civilian's extraction experience is the entire value proposition of the constraint — the same institutional process appears beneficial from the beneficiary position and harmful from the victim position. The piton perspective reveals that international engagement is increasingly theater: as regime consolidation succeeds, international mechanisms persist but enforce less, creating an appearance of accountability without substance.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from each actor's structural relationship to the extraction flow. The regime as beneficiary with arbitrage options experiences low d (~0.15-0.20) — can trade reconstruction participation between backers, experiencing the constraint as coordination benefit. Civilian populations as trapped victims experience maximum d (~0.95) — no exit capacity, bear full costs, experience maximum effective extraction. Opposition groups as organized but constrained victims experience high d (~0.70-0.80) — have some agency through organizing but constrained by military defeat and international abandonment. International backers (Russia, Iran) as powerful but constrained beneficiaries experience moderate d (~0.35-0.45) — extract geopolitical benefit but constrained by Syria's ongoing structural weakness and international pressure. Civil society as organized with mobile exit options experiences lower d (~0.40-0.50) — can exit to regional operation and document remotely. International institutions as arbitrage-positioned beneficiaries experience low d (~0.20) — can shift engagement patterns to maintain legitimacy. The piton perspective's low extractiveness reflects that performative mechanisms preserve institutional appearances without substantive extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL MANDATE TRAP: The Syrian state capacity recovery constraint contains a mandate inversion: the international community's mandate is to rebuild state capacity for civilian benefit, but the structural dynamics produce regime consolidation for regime benefit. This mandate trap is resolved through the tangled rope classification at multiple institutional levels — genuine coordination needs (displaced return, security integration, economic recovery) coexist with asymmetric extraction (regime capture of state monopoly, international positioning extraction, civilian suppression). The constraint avoids false snare classification (which would suggest no coordination function) by acknowledging that reconstruction does address real coordination problems. It avoids false rope classification by acknowledging that the beneficiaries capture the gains asymmetrically. The mandatrophy is resolved not by finding the 'true' type but by recognizing that tangled rope IS the answer: genuine coordination layered with systematic extraction, enforced through security apparatus and international performance. The scaffold perspective (civil society with sunset logic) represents the alternative mandate pathway — if civil society mechanisms prove sufficient to document abuses and build transitional capacity, external pressure could eventually force power-sharing arrangements. But the current trajectory (extractiveness declining due to ossification, theater increasing due to performative engagement) suggests entrenchment rather than transition — the mandate is being inverted, not fulfilled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reconstruction_timeline_entrenchment,
    'Does extended reconstruction period entrench regime consolidation beyond the point of reversibility, converting the constraint from temporary to permanent?',
    'Tracking institutional ossification metrics: staff retention in security apparatus, property restitution timelines, opposition political participation windows. If key transition mechanisms close after 3-5 years, entrenchment is occurring.',
    'If entrenchment occurs: constraint transitions from tangled_rope with sunset logic (scaffold perspective) to permanent snare (extractive consolidation becomes structural). Determines whether international engagement can influence outcome or merely delays inevitable regime hardening.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_timeline_entrenchment, empirical, 'Whether extended reconstruction entrenches regime consolidation').

omega_variable(
    international_conditionality_enforcement,
    'Are international reconstruction funds actually conditional on accountability measures and opposition inclusion, or are conditions performative while extraction proceeds unchecked?',
    'Tracking disbursal patterns: proportion of funds withheld for non-compliance, cross-referencing with reported violations. Interviews with reconstruction finance officials on enforcement mechanisms and political barriers.',
    'If enforcement is real: rope perspective gains legitimacy — reconstruction is genuine coordination with extractive cost but with accountability framework. If performative: piton perspective confirmed — institutional theater masks unconditional extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_conditionality_enforcement, empirical, 'Whether international conditionality is enforced or performative').

omega_variable(
    opposition_coalition_viability,
    'Can opposition groups organize across exile/internal divides to constitute a credible alternative governance structure, or does fragmentation lock them into powerless trapped status?',
    'Monitoring opposition coalition formation, cross-border coordination capacity, international recognition patterns. If coalitions form and gain institutional recognition, powerless agents could transition to organized status.',
    'If opposition organizes: perspectives shift from powerless (snare) toward organized (tangled_rope) — changes classification from pure extraction to mixed. If opposition remains fragmented: trap deepens — classification confirms snare for all victim-position perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coalition_viability, empirical, 'Whether opposition can achieve coalition viability').

omega_variable(
    external_pressure_consistency,
    'Do external actors (US, EU, Arab League) maintain consistent pressure for accountability and opposition inclusion, or does pressure collapse when geopolitical priorities shift?',
    'Tracking sanctions enforcement, diplomatic isolation mechanisms, reconstruction funding conditionality over 5-10 year horizon. Watch for sudden normalization (UAE-Syria diplomatic relations, Arab League readmission) that signals pressure collapse.',
    'If pressure weakens: regime consolidation deepens, snare characteristics become permanent. If pressure maintains: scaffold sunset logic remains viable — alternative pathways (transitional justice, power-sharing) remain structurally possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(external_pressure_consistency, preference, 'Whether external pressure for accountability persists').

omega_variable(
    economic_viability_reconstruction,
    'Can Syria''s economy recover sufficiently to sustain regime consolidation without permanent international subsidy dependency, or does reconstruction fail and force reconfiguration?',
    'Tracking economic indicators: currency stability, private investment return, sectoral recovery rates. If economy remains dependent on external transfers after 7+ years, structural reconfiguration becomes necessary.',
    'If economy fails: regime''s arbitrage options collapse — cannot maintain security apparatus without funds. Forces either international normalization (pressure softens) or state failure (constraint reverts to snare under regime collapse). If economy recovers: regime consolidation becomes self-sustaining — snare characteristics become permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_viability_reconstruction, empirical, 'Whether reconstruction achieves economic self-sufficiency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(syrian_state_capacity_recovery, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(syr_cap_tr_t0, syrian_state_capacity_recovery, theater_ratio, 0, 0.55).
narrative_ontology:measurement(syr_cap_tr_t3, syrian_state_capacity_recovery, theater_ratio, 3, 0.62).
narrative_ontology:measurement(syr_cap_tr_t6, syrian_state_capacity_recovery, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(syr_cap_be_t0, syrian_state_capacity_recovery, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(syr_cap_be_t3, syrian_state_capacity_recovery, base_extractiveness, 3, 0.65).
narrative_ontology:measurement(syr_cap_be_t6, syrian_state_capacity_recovery, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(syrian_state_capacity_recovery, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(syrian_state_capacity_recovery, 0.18).
narrative_ontology:affects_constraint(syrian_state_capacity_recovery, lebanese_hezbollah_integration).
narrative_ontology:affects_constraint(syrian_state_capacity_recovery, iraqi_shia_militias_coordination).
narrative_ontology:affects_constraint(syrian_state_capacity_recovery, gaza_supply_corridor_control).
narrative_ontology:affects_constraint(syrian_state_capacity_recovery, eastern_mediterranean_energy_exploration).

% DUAL FORMULATION NOTE:
% Syrian state capacity recovery is the coordination hub for a regional constraint family. Upstream constraints include civil war termination and international intervention structures; downstream constraints include specific regional network deployments (Hezbollah integration, Iraqi militia coordination, energy exploration). Each downstream constraint inherits extractiveness from this hub: as state capacity consolidates, regional extraction mechanisms become sustainable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(syrian_state_capacity_recovery, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
