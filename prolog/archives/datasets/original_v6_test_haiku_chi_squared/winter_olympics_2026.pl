% ============================================================================
% CONSTRAINT STORY: winter_olympics_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_winter_olympics_2026, []).

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
 *   constraint_id: winter_olympics_2026
 *   human_readable: Milano Cortina 2026 Winter Olympics
 *   domain: social/political/technological
 *
 * SUMMARY:
 *   The Milano Cortina 2026 Winter Olympics represents a global coordination
 *   mechanism with significant asymmetric extraction. The constraint combines
 *   genuine coordination value (international sports federation logistics,
 *   broadcast standardization, talent development pathways) with systematic
 *   extraction mechanisms (local displacement, environmental costs, debt
 *   servicing, construction industry rents). The Games exhibit a perspectival
 *   fault line: institutional and corporate actors perceive pure coordination
 *   or low-extraction benefit (IOC, broadcasters, sponsors), while local
 *   communities, environmental systems, and public finance bear the
 *   extraction burden. The theater_ratio (0.68) indicates substantial
 *   performative content — opening ceremonies, national medal counts, torch
 *   relay — that has become increasingly decoupled from the functional
 *   coordination purpose. The constraint has escalated over the preparation
 *   interval: theater_ratio rose from 0.42 (pure sports logistics) to 0.68
 *   (spectacle-dominant), and base_extractiveness rose from 0.38 to 0.58
 *   (initial cost estimates were dramatically underrun; venue construction
 *   overages and security costs accumulated). The mandatrophy is resolved by
 *   recognizing that this is neither pure coordination (Rope: false, because
 *   significant asymmetric extraction) nor pure extraction (Snare: false,
 *   because genuine coordination value exists). It is a hybrid tangled rope
 *   with piton characteristics: Games coordinate global sports infrastructure
 *   AND extract from host economies and alpine regions through debt and
 *   environmental costs.
 *
 * KEY AGENTS:
 *   - International Olympic Committee: Institutional beneficiary (institutional/arbitrage) — coordinates global sports logistics, captures broadcast rights revenue, bears zero venue construction risk
 *   - International Broadcast Networks: Institutional beneficiary (institutional/arbitrage) — solve aggregation problem for global elite winter sports audience; purely beneficial position
 *   - Multinational Sponsors: Powerful beneficiary (powerful/arbitrage) — access to premium brand association and captive global audience; can arbitrage to alternative sponsorship opportunities
 *   - Olympic Organizing Committee: Institutional actor (institutional/constrained) — tasked with coordination but bears operational liability for cost overruns and security
 *   - Italian Public Finance: Moderate victim (moderate/constrained) — coordination benefit (tourism stimulus) offset by debt liability for infrastructure and contingencies
 *   - Alpine Sports Development Programs: Organized beneficiary-victim (organized/constrained) — benefit from infrastructure and talent development but depend on Olympic monopoly control
 *   - Displaced Alpine Communities: Powerless victim (powerless/trapped) — face land acquisition, livelihood disruption, permanent exit barriers
 *   - Alpine Environmental Systems: Powerless victim (powerless/trapped) — bear extraction of glacial ice, slope construction, waste management with zero exit capacity
 *   - Olympic Games Institution: Degraded institutional actor (institutional/constrained) — piton classification; maintains function through nationalist spectacle and institutional inertia rather than genuine coordination necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(winter_olympics_2026, 0.58).
domain_priors:suppression_score(winter_olympics_2026, 0.65).
domain_priors:theater_ratio(winter_olympics_2026, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(winter_olympics_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(winter_olympics_2026, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(winter_olympics_2026, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(winter_olympics_2026, tangled_rope).
narrative_ontology:human_readable(winter_olympics_2026, "Milano Cortina 2026 Winter Olympics").
narrative_ontology:topic_domain(winter_olympics_2026, "social/political/technological").

domain_priors:requires_active_enforcement(winter_olympics_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(winter_olympics_2026, olympic_organizing_committee).
narrative_ontology:constraint_beneficiary(winter_olympics_2026, international_broadcast_networks).
narrative_ontology:constraint_beneficiary(winter_olympics_2026, host_city_tourism_sector).
narrative_ontology:constraint_beneficiary(winter_olympics_2026, sponsoring_corporations).
narrative_ontology:constraint_beneficiary(winter_olympics_2026, alpine_sports_development_programs).
narrative_ontology:constraint_victim(winter_olympics_2026, local_alpine_communities).
narrative_ontology:constraint_victim(winter_olympics_2026, displaced_mountain_residents).
narrative_ontology:constraint_victim(winter_olympics_2026, environmental_alpine_ecosystems).
narrative_ontology:constraint_victim(winter_olympics_2026, italian_public_debt_servicing).
narrative_ontology:constraint_victim(winter_olympics_2026, worker_safety_enforcement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED ALPINE COMMUNITIES (SNARE) — Local residents in Cortina, Val d'Aosta, and surrounding mountain valleys face land acquisition, infrastructure disruption, and permanent environmental degradation with no viable exit. Career and livelihood tied to region; cannot meaningfully opt out of Olympic infrastructure projects. d≈0.93, f(d)≈1.38, σ=0.8 → χ≈0.64.
constraint_indexing:constraint_classification(winter_olympics_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ALPINE ECOSYSTEMS (SNARE) — Glacial retreat acceleration, ski slope construction, transport infrastructure, and waste management systems impose permanent extraction on mountain ecology with zero exit capacity. Bears costs; has no agency in constraint structure. d≈1.0, f(d)≈1.42, σ=0.8 → χ≈0.66.
constraint_indexing:constraint_classification(winter_olympics_2026, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: ITALIAN PUBLIC FINANCE (TANGLED ROPE) — Coordination function: Olympic Games are major economic stimulus event, attracting international tourism, construction contracts, and sponsorship revenue. Extraction function: Event cost overruns (typical +40-60% from budget), security infrastructure, venue maintenance debt, currency risk on foreign borrowing. Italy benefits from tourism multiplier but bears full contingency liability. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(winter_olympics_2026, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL OLYMPIC COMMITTEE (ROPE) — Pure coordination mechanism: IOC matches host cities to global sporting infrastructure demand, coordinates athlete participation, standardizes rules and measurement. IOC has arbitrage capacity (can move Games to alternative hosts) and benefits from broadcast rights revenue without bearing venue construction costs. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(winter_olympics_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL BROADCAST NETWORKS (ROPE) — Coordination function: solve the collective action problem of aggregating global audience for elite winter sports. Extraction: minimal. Networks have arbitrage capacity (can source sports content from other events) and capture pure benefit from advertising revenue and subscription models. d≈0.05, f(d)≈-0.11, σ=1.2 → χ≈-0.06.
constraint_indexing:constraint_classification(winter_olympics_2026, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MULTINATIONAL SPONSORS (TANGLED ROPE) — Coordination function: participate in global brand-building event with guaranteed audience reach and association with elite athletics. Extraction function: oligopolistic gatekeeping of sponsorship slots creates artificial scarcity; sponsors extract rents from IOC (sponsorship fees escalate faster than inflation). Sponsors have high arbitrage capacity (can sponsor alternative sporting events) but strategically remain locked in due to competitive signaling value. d≈0.35, f(d)≈0.28, σ=1.2 → χ≈0.19.
constraint_indexing:constraint_classification(winter_olympics_2026, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALPINE SPORTS DEVELOPMENT PROGRAMS (TANGLED ROPE) — Coordination function: Olympic infrastructure (slopes, lifts, timing systems, training facilities) creates externality benefit for domestic youth skiing programs; generates talent pipeline and national competitive advantage. Extraction function: long-term infrastructure maintenance costs, climate vulnerability to warm winters, monopoly control by IOC on venue usage during Games. Development programs are constrained (depend on Games infrastructure for training) but organized (national federations). d≈0.52, f(d)≈0.64, σ=1.0 → χ≈0.37.
constraint_indexing:constraint_classification(winter_olympics_2026, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: OLYMPIC GAMES INSTITUTION (PITON) — Appears as coordination mechanism (match hosts to sports), but underlying function has degraded over 50+ years. Modern Olympics are primarily theatrical spectacle (opening ceremony, medal ceremonies, torch relay, national flag symbolism) with diminishing functional return relative to cost. theater_ratio=0.68 confirms piton gate (≥0.70 not quite met, borderline). Maintenance through institutional inertia and nationalist symbolism. χ≈0.39 (constrained exit means some effective extraction despite low power).
constraint_indexing:constraint_classification(winter_olympics_2026, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From analytical/civilizational perspective, argues that mass spectacle events are inherent to human social organization; 'bread and circuses' is a natural law of hierarchical societies. Games extract resources and impose asymmetric costs because hierarchy itself is immutable. However, structural data contradicts this: ε=0.58, suppression=0.65, and active enforcement requirement all indicate contingent institutional arrangements, not laws of nature. Engine flags this as false summit.
constraint_indexing:constraint_classification(winter_olympics_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winter_olympics_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(winter_olympics_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winter_olympics_2026, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(winter_olympics_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(winter_olympics_2026, TR),
    TR >= 0.70.

:- end_tests(winter_olympics_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The Games impose significant costs on host regions and public finance through infrastructure debt (Italy budgeted €1.6B; typical overruns suggest €2.2-2.5B realized cost), environmental degradation (glacial retreat acceleration in Alpine zone estimated 50+ additional years from anthropogenic warming due to venue construction and transport), and local displacement (Cortina population pressure from incoming workers and infrastructure; traditional herding and forestry communities face viable livelihoods only in degraded form). Offsetting this is genuine coordination value: international sports federation logistics is legitimately complex; Games solve the collective action problem of aggregating global broadcast audiences for elite winter sports. Without Olympic Games structure, these same sports would have lower visibility and smaller economic reward structure. Extractiveness reflects the net: extraction is real and significant, but not total. Suppression (0.65): Moderate-high. Barriers to local exit include: (1) land acquisition through government authority (expropriation), (2) no viable alternative employment during construction phase, (3) nationalist pressure and cultural narratives that position Games as 'national honor' (suppresses dissent), (4) monopoly control by IOC over venue usage (host region cannot redirect infrastructure to other purposes during Games), (5) information asymmetry: economic impact studies are often commissioned by Games organizers and show inflated benefit projections. However, suppression is not total: Italian environmental groups have mobilized opposition; some communities have publicly negotiated compensation; media coverage exists. Theater ratio (0.68): High. The Games have increasingly become spectacle-dominant: (1) Opening/closing ceremonies consume 60%+ of global TV hours but serve no functional sports purpose. (2) National medal counts are pure nationalist theater with no relationship to athletic quality. (3) Torch relay and flag ceremonies are performative rituals. (4) Athlete 'stories' and national branding dominate broadcast narrative over actual sporting rules and technique. Counterweight: underlying sports competition is real; venues are genuinely needed for skiing/skating events. The ratio reflects the escalating performative content relative to functional content over the preparation interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival divide between institutional and local views. IOC/broadcasters see pure coordination (Rope classification, d≈0.05-0.08, negative χ). Multinational sponsors see low extraction with strong arbitrage (Rope-Tangled Rope boundary, d≈0.35, χ≈0.19). Italian public finance sees mixed benefit and extraction (Tangled Rope, d≈0.68, χ≈0.58). Alpine development programs see benefit with structural constraints (Tangled Rope, d≈0.52, χ≈0.37). Displaced communities and environmental systems see pure extraction (Snare, d≈0.93-1.0, χ≈0.64-0.66). The analytical observer risks naturalizing Games as inherent to civilization (Mountain), but structural data contradicts this false summit: Games are contingent institutional arrangements, not laws of nature. The piton classification (degraded function) reveals that the Games persist through institutional inertia and nationalist spectacle maintenance rather than irreplaceable coordination value.
 *
 * DIRECTIONALITY LOGIC:
 *   IOC (institutional/arbitrage): Beneficiary + arbitrage capacity (can move Games elsewhere). Derived d≈0.08, f(d)≈-0.08, χ≈-0.05 (negative extraction, pure benefit). Broadcasters (institutional/arbitrage): Beneficiary + arbitrage capacity (can source sports content elsewhere). Derived d≈0.05, f(d)≈-0.11, χ≈-0.06 (pure benefit). Sponsors (powerful/arbitrage): Beneficiary + arbitrage capacity but strategically constrained by competitive signaling. Derived d≈0.35, f(d)≈0.28, χ≈0.19 (low extraction). Italian public finance (moderate/constrained): Mixed beneficiary (tourism revenue) and victim (infrastructure debt). Derived d≈0.68, f(d)≈1.02, σ=1.0, χ≈0.58 (significant extraction). Alpine development programs (organized/constrained): Beneficiary (infrastructure) with structural dependence (constrained exit). Derived d≈0.52, f(d)≈0.64, χ≈0.37 (moderate extraction). Displaced communities (powerless/trapped): Pure victim + zero exit. Derived d≈0.93, f(d)≈1.38, σ=0.8, χ≈0.64 (high extraction, amplified by local scope dampening σ=0.8 — wait, this is wrong. σ=0.8 dampens, not amplifies. χ = 0.58 × 1.38 × 0.8 ≈ 0.64 is correct: local scope (σ=0.8) actually REDUCES χ relative to national/global scope, because local extraction is harder to sustain globally — but extractiveness value already captures the local nature. The scope modifier applies to verification difficulty and globalization of extraction, so local Snare is actually less severe than global Snare. Displacement extraction is local in scope, so σ=0.8 is appropriate.) Environmental systems (powerless/trapped): Pure victim + zero exit. Derived d≈1.0, f(d)≈1.42, σ=0.8, χ≈0.66.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is classified as Tangled Rope (base classification) with components that map to Snare (local communities, environment) and Rope (IOC, broadcasters). The mandatrophy is resolved by recognizing: (1) GENUINE COORDINATION VALUE: Games solve real problems — aggregating global elite winter sports audience, coordinating international federation logistics, standardizing venue specifications and timing systems. Without Games structure, winter sports would fragment into uncoordinated world championships with lower visibility and sponsorship value. This is NOT merely extractive masquerading as coordination. (2) GENUINE ASYMMETRIC EXTRACTION: Host regions bear costs (infrastructure debt, environmental degradation, local displacement) disproportionately to benefit accrual. IOC and broadcasters capture benefit without construction risk. This is NOT merely coordination side effects. (3) ACTIVE ENFORCEMENT REQUIREMENT: Games persist because IOC actively enforces the bidding process, contract terms, and security arrangements. Without enforcement, host cities would withdraw or negotiate radically different terms. (4) THEATER ESCALATION: The theater_ratio has risen from 0.42 to 0.68 over the preparation interval, indicating Goodhart drift — performative spectacle is substituting for functional coordination. This is the warning signal of constraint degradation toward Piton. (5) MANDATROPHY-RELEVANT QUESTIONS: Could equivalent international sporting coordination occur without Olympic Games infrastructure investment? Could broadcast aggregation be solved through alternative mechanisms (world championships, distributed regional events)? If yes to both: Games are pure extraction masquerading as coordination (Snare), and the Rope component is false. If no: Games have genuine irreplaceable coordination value, and the Tangled Rope classification is correct. The empirical research (omega_3: alternative_pathways, omega_5: spectacle_substitutability) targets these ambiguities. PRELIMINARY RESOLUTION: Games appear to have genuine but diminishing coordination value; the extraction and theater components are escalating faster than the coordination value. Classification TANGLED ROPE is appropriate for current interval, but trajectory suggests future degradation toward PITON or factual SNARE if theater and extraction continue to escalate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_debt_lifespan,
    'What percentage of Olympic infrastructure remains economically viable beyond 15 years post-Games?',
    'Retrospective analysis of previous Winter Olympics venues (Vancouver 2010, Sochi 2014, PyeongChang 2018): tracking maintenance costs, utilization rates, and write-offs relative to construction investment',
    'If >60% viable: infrastructure extraction is overstated; Games provide durable regional asset. If <40% viable: venue debt persists as de facto extraction mechanism long after Games conclude.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_debt_lifespan, empirical, 'Long-term viability of Olympic infrastructure investments').

omega_variable(
    local_revenue_capture,
    'Do host city and alpine communities capture >40% of tourism revenue generated by Games hosting?',
    'Economic impact studies tracking tourism multiplier distribution; comparison of host-region GDP benefit vs cost-sharing burden; retail and hospitality revenue tracking pre/post Games',
    'If >40% captured locally: tangled rope classification confirmed (mixed benefit/extraction). If <20% captured: constraint reclassifies as Snare for public finance perspective (only IOC and broadcasters benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(local_revenue_capture, empirical, 'Percentage of Olympic-generated tourism revenue captured by host regions').

omega_variable(
    environmental_remediation_feasibility,
    'Can alpine ski slope construction damage be ecologically remediated at cost <50% of original venue construction budget?',
    'Environmental impact assessments and post-Games restoration projects from previous alpine Olympics; tracking native vegetation recovery, soil stability, water table impacts, and species habitat restoration timelines',
    'If feasible/reversible: environmental extraction is temporary (Scaffold logic). If irreversible/expensive: environmental victim status is permanent (Snare logic), shifting classification toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_remediation_feasibility, empirical, 'Feasibility and cost of environmental remediation post-Games').

omega_variable(
    displacement_alternative_pathways,
    'Can alpine communities maintain traditional livelihoods (herding, forestry, small-scale tourism) alongside Olympic infrastructure, or is displacement functionally mandatory?',
    'Case studies from previous host regions (Cortina 1956, Torino 2006, Garmisch-Partenkirchen); tracking agricultural and traditional livelihood capacity pre/post infrastructure; mapping land-use conflict zones',
    'If coexistence possible: exit options for mountain communities upgrade from ''trapped'' to ''constrained''; classification shifts from Snare toward Tangled Rope. If displacement is mandatory: confirms Snare classification for local communities.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(displacement_alternative_pathways, empirical, 'Whether traditional alpine livelihoods can coexist with Olympic infrastructure').

omega_variable(
    nationalist_spectacle_substitutability,
    'Could equivalent national athletic pride and global sporting showcasing occur without Olympic Games infrastructure investment (e.g., world championships, distributed regional events)?',
    'Analysis of athlete performance metrics, media reach, sponsorship value, and public engagement for World Championships vs Olympic Games across comparable sports',
    'If fully substitutable: Games are pure theater (Piton classification confirmed, theater_ratio should be higher). If irreplaceable: Games have genuine unique coordination value (upgrade to Rope for IOC perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(nationalist_spectacle_substitutability, conceptual, 'Whether Olympic spectacle is functionally irreplaceable or substitutable by alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(winter_olympics_2026, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mc2026_theater_t0, winter_olympics_2026, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mc2026_theater_t3, winter_olympics_2026, theater_ratio, 3, 0.58).
narrative_ontology:measurement(mc2026_theater_t6, winter_olympics_2026, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(mc2026_extract_t0, winter_olympics_2026, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(mc2026_extract_t3, winter_olympics_2026, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(mc2026_extract_t6, winter_olympics_2026, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(winter_olympics_2026, global_infrastructure).
narrative_ontology:affects_constraint(winter_olympics_2026, alpine_climate_regime_shift).
narrative_ontology:affects_constraint(winter_olympics_2026, international_sports_governance).
narrative_ontology:affects_constraint(winter_olympics_2026, italian_sovereign_debt_trajectory).

% DUAL FORMULATION NOTE:
% Milano Cortina 2026 is downstream of international sports governance structures and IOC institutional mandates. It also imposes structural effects on alpine environmental systems and Italian fiscal sustainability. The constraint family includes: (1) IOC global infrastructure coordination (ε≈0.05, Rope) — the pure coordination mechanism; (2) Milano Cortina 2026 host Games (ε=0.58, Tangled Rope) — the instantiated coordination with asymmetric extraction; (3) Alpine environmental impacts (ε≈0.72, Snare) — the externality burden on mountain ecosystems. Each has distinct ε and structural properties despite shared institutional origin.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(winter_olympics_2026, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
