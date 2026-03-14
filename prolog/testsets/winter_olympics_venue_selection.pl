% ============================================================================
% CONSTRAINT STORY: winter_olympics_venue_selection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_winter_olympics_venue_selection, []).

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
 *   constraint_id: winter_olympics_venue_selection
 *   human_readable: Winter Olympics Venue Selection Process
 *   domain: political_economy/sports_infrastructure
 *
 * SUMMARY:
 *   The Winter Olympics venue selection process exemplifies how institutional
 *   authority (IOC) coordinates genuine international athletic competition
 *   while simultaneously extracting rents from host nations and imposing
 *   externalities on displaced populations. The constraint combines real
 *   coordination function (matching global winter sports facilities with
 *   international competition standards) with structural asymmetries that
 *   concentrate benefits among IOC decision-makers, construction contractors,
 *   and host-nation political elites while distributing costs across
 *   communities, environments, and competing bidders. The constraint's
 *   extractiveness has increased from 0.35 to 0.58 over a typical Olympic
 *   cycle: initial bidding costs are moderate, but venue construction costs,
 *   displacement externalities, and post-Olympic debt accumulate. Theater
 *   ratio has risen from 0.52 to 0.68, indicating that the performative
 *   aspects of venue selection (bidding presentations, site visits, IOC
 *   voting ceremony) have become more elaborate relative to the functional
 *   requirement of identifying suitable winter sports facilities. The same
 *   set of structural realities appears as coordination (rope) from IOC
 *   perspective, extraction (snare) from displaced populations, mixed
 *   outcomes (tangled rope) from host governments and competing nations,
 *   degraded ritual (piton) from athlete perspectives, and potentially a
 *   false natural law (mountain) from observers who naturalize geographic
 *   constraints into immutable Olympic requirements.
 *
 * KEY AGENTS:
 *   - International Olympic Committee: Primary beneficiary (institutional/arbitrage) — controls venue selection authority, extracts hosting fees, negotiates infrastructure mandates with asymmetric bargaining power
 *   - Displaced Communities: Primary victim (powerless/trapped) — face involuntary relocation, environmental degradation, infrastructure disruption with no exit options; concentrated in local geographic scope
 *   - Host Nation Government: Secondary beneficiary and victim (powerful/mobile) — captures infrastructure investment benefits but bears construction costs, debt service, and political accountability; powerful in principle but politically trapped by Olympic commitment
 *   - Competing Bidding Nations: Secondary victims (moderate/constrained) — invest in elaborate bidding presentations with low probability of selection; exit costly due to sunk costs and political commitment; only one benefits, rest bear bidding costs with no return
 *   - Construction Contractors: Secondary beneficiary (powerful/arbitrage) — capture construction contracts, benefit from cost overruns, minimum competition due to Olympic specifications
 *   - Athlete Community: Incidental actor (analytical/analytical) — need functional venues (coordination function) but benefit/harm distribution from selection process is decoupled from athlete experience
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(winter_olympics_venue_selection, 0.58).
domain_priors:suppression_score(winter_olympics_venue_selection, 0.65).
domain_priors:theater_ratio(winter_olympics_venue_selection, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(winter_olympics_venue_selection, extractiveness, 0.58).
narrative_ontology:constraint_metric(winter_olympics_venue_selection, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(winter_olympics_venue_selection, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(winter_olympics_venue_selection, tangled_rope).
narrative_ontology:human_readable(winter_olympics_venue_selection, "Winter Olympics Venue Selection Process").
narrative_ontology:topic_domain(winter_olympics_venue_selection, "political_economy/sports_infrastructure").

domain_priors:requires_active_enforcement(winter_olympics_venue_selection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(winter_olympics_venue_selection, ioc_decision_authority).
narrative_ontology:constraint_beneficiary(winter_olympics_venue_selection, host_nation_political_elites).
narrative_ontology:constraint_beneficiary(winter_olympics_venue_selection, construction_contractors).
narrative_ontology:constraint_victim(winter_olympics_venue_selection, displaced_communities).
narrative_ontology:constraint_victim(winter_olympics_venue_selection, environmental_systems).
narrative_ontology:constraint_victim(winter_olympics_venue_selection, competing_bidding_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED COMMUNITIES (SNARE) — Local residents in selected venues face involuntary relocation, environmental degradation, and infrastructure disruption with no meaningful exit options or compensation. Cannot prevent selection, cannot block construction, cannot exit the geographic region without losing property. Maximum suppression and extraction — bears full cost of Olympic imposition.
constraint_indexing:constraint_classification(winter_olympics_venue_selection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: COMPETING HOST NATIONS (TANGLED ROPE) — Nations bidding for Olympics face genuine coordination benefits (international prestige, infrastructure investment, tourism potential) alongside asymmetric extraction (IOC venue fees, mandatory spending, construction debt). Constrained exit due to sunk bidding costs and domestic political commitment. High suppression because once committed, cannot withdraw without domestic political cost.
constraint_indexing:constraint_classification(winter_olympics_venue_selection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INTERNATIONAL OLYMPIC COMMITTEE (ROPE) — Experiences venue selection as pure coordination mechanism: matching host capacity with athlete requirements, distributing prestige across nations, managing global Olympic logistics. Net beneficiary with arbitrage options — can leverage bidding competition to extract higher concessions. IOC sees minimal extraction directed at itself; maximum arbitrage access.
constraint_indexing:constraint_classification(winter_olympics_venue_selection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HOST NATION GOVERNMENT (TANGLED ROPE) — Powerful actor but structurally committed to Olympic delivery. Genuine coordination function (infrastructure development, international relations) alongside extraction (construction cost overruns, debt servicing, environmental externalizing). Mobile in principle but politically trapped by public commitment. Experiences mixed extraction and benefit depending on implementation.
constraint_indexing:constraint_classification(winter_olympics_venue_selection, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: OLYMPIC ATHLETE COMMUNITY (PITON) — Athletes benefit from Olympic competition hosting but their genuine functional need (quality venues, athlete safety) is decoupled from the venue selection theater. The selection process (bidding presentations, site visits, IOC voting) is substantially performative — real venues are determined by geography, climate, and infrastructure availability, not IOC selection drama. Athletes would achieve same functional outcome with much lower theater and extraction.
constraint_indexing:constraint_classification(winter_olympics_venue_selection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational/universal perspective, winter sports venue selection appears immutably constrained by geography (adequate snow, altitude, temperature, topography). Natural constraints on where winter athletics can occur seem like irreducible limits. However, structural data contradicts mountain classification — artificial restrictions (IOC authority, cost floors, bidding theater) are contingent institutional arrangements, not natural laws.
constraint_indexing:constraint_classification(winter_olympics_venue_selection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winter_olympics_venue_selection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(winter_olympics_venue_selection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winter_olympics_venue_selection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(winter_olympics_venue_selection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(winter_olympics_venue_selection, TR),
    TR >= 0.70.

:- end_tests(winter_olympics_venue_selection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. The IOC venue selection process combines genuine coordination (matching facilities to competition standards, distributing prestige, managing logistics) with systematic extraction. The extraction mechanism operates through: (1) IOC fee structures that concentrate revenue; (2) mandatory infrastructure specifications that inflate host costs; (3) construction contracts awarded through restricted competition; (4) shifting externalities (displacement, environmental degradation) away from IOC to host communities. Initial extractiveness is lower (0.35) because early bidding phases involve genuine coordination discussion and host autonomy. By mid-cycle (0.50), construction begins and displacement/environmental costs materialize. Final extractiveness (0.58) reflects accumulated debt, locked-in contracts, and irreversible environmental damage. Suppression (0.65): High. Suppression mechanisms include: institutional authority (IOC has near-total venue control), information asymmetry (detailed cost projections not disclosed pre-selection), regulatory capture (host governments cannot renegotiate Olympic specifications), geographic entrapment (affected communities cannot relocate before venue selection occurs), political commitment (domestic constituencies pressure continuation despite cost overruns), and institutional inertia (Olympic hosting perceived as prestige necessitating acceptance). Theater ratio (0.68): High and increasing. The performative aspects include elaborate bidding presentations by competing nations, IOC site visits staged for media, voting ceremonies designed for global broadcast, and post-selection celebration events — all substantially divorced from functional venue determination. Venues are determined by geography and existing infrastructure; the theater adds elaboration without functional value. Theater increases over the cycle as host governments invest more in ceremonial displays to justify costs to domestic constituencies.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a systematic perspectival gap between beneficiaries and victims, and between institutional and community-level perspectives. The IOC experiences pure coordination (Rope) — their functional requirement (match sites to competition specs) is satisfied cleanly, and they perceive bidding competition as healthy. Host governments experience Tangled Rope — genuine infrastructure benefits alongside debt burden and political commitment. Competing nations experience Tangled Rope shifted toward Snare — high bidding investment with 95% failure rate and no benefit. Displaced communities experience Snare — total extraction with no coordination benefit and no exit. Athletes experience Piton — the functional Olympic competition could occur in 2-3 well-maintained existing venues globally, making most of the venue selection theater unnecessary. The analytical observer risks seeing Mountain (geographic constraints on winter sports appear immutable) but structural analysis reveals this as naturalization: alternative venues exist and rotation is feasible, but institutional arrangements make rotation appear impossible.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim status and exit options. IOC benefits and has arbitrage options (d ≈ 0.05, low extraction experienced). Host nation governments benefit but are politically trapped (d ≈ 0.50, moderate extraction). Competing nations are victims with high-cost-of-entry exit (d ≈ 0.75, high extraction). Displaced communities are victims with no exit (d ≈ 0.95, maximum extraction). The sigmoid f(d) maps these to experienced extractiveness at each perspective: beneficiaries with arbitrage see low/negative chi; victims see high chi. Geographic scope modifier (σ) is local for displacement (σ=0.8) and global for IOC (σ=1.2), which affects final chi calculations. Host governments at national scope (σ=1.0) experience chi between local and global perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by decomposing the Olympics venue selection into distinct structural functions. The genuinely coordinating function (Olympic facility matching, international logistics, athlete support) is real and produces Rope-type benefits. The extractive mechanism (IOC rent capture, contractor cost-plus, displacement externalities, bidding theater) is structurally distinct. The Tangled Rope classification correctly captures that both functions coexist: real coordination enabling asymmetric extraction. The constraint does NOT collapse to pure coordination (Rope) because displacement is structural and systematic, not accidental. The constraint does NOT collapse to pure extraction (Snare) because infrastructure does provide some host-nation benefit and genuine Olympic logistics do require coordination. The Tangled Rope classification prevents mischaracterization: it is not coordination masking extraction (Snare wearing Rope clothes), nor is it benign cooperation with minor extraction (Rope with extraction). It is hybrid: genuine coordination function that structurally concentrates extraction toward an institutional beneficiary (IOC).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_mechanism_definitional,
    'Does IOC venue control constitute genuine coordination extraction or pure regulatory rent-seeking?',
    'Comparative analysis of Olympic outcomes with and without IOC authority; counterfactual modeling of alternative coordination structures; historical trend in host nation debt-to-benefit ratios',
    'If coordination: classification shifts toward Rope from more perspectives. If rent-seeking: classification shifts toward Snare/Tangled Rope for national governments.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_mechanism_definitional, conceptual, 'Whether IOC authority coordinates or extracts').

omega_variable(
    displacement_scope_quantification,
    'What proportion of displaced persons constitute the ''victims'' group? Does displacement represent structural targeting or incidental externality?',
    'Historical census data on displacement across 10+ Olympic cycles; causal analysis of venue selection versus displacement timing; comparison to infrastructure projects of equivalent scale in non-Olympic contexts',
    'If ≥5000 displaced per venue: structural victimization (Snare feature confirmed). If <500: incidental externality (reduces suppression, reclassifies toward Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(displacement_scope_quantification, empirical, 'Displacement scale and whether structural or incidental').

omega_variable(
    host_nation_benefit_realization,
    'Do host nations realize positive long-term infrastructure and economic benefits, or are benefits captured by IOC/contractors with nations bearing structural costs?',
    '20-year post-Olympic economic analysis: debt service costs versus tourism revenue, infrastructure utilization, employment effects; comparison of host GDP growth to non-host peer nations; property ownership patterns post-Olympics',
    'If positive: Tangled Rope with net benefit to nation (mixed). If negative: Tangled Rope with net extraction from nation (Snare-shifting).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(host_nation_benefit_realization, empirical, 'Whether host nations realize net benefits from Olympic hosting').

omega_variable(
    environmental_baseline_recovery,
    'Do Olympic venue environments recover to pre-construction baselines within 10 years, or do environmental costs represent permanent extraction?',
    'Ecological monitoring post-Olympics: soil recovery, water quality restoration, species habitat recovery; comparison to environmental degradation from equivalent-scale infrastructure projects; cost to restore vs. original construction cost',
    'If recovery complete: environmental externality classifiable as temporary. If incomplete: permanent environmental extraction (increases suppression metric).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_baseline_recovery, empirical, 'Whether environmental degradation from Olympics is reversible').

omega_variable(
    bidding_coercion_mechanism,
    'Does the bidding process constitute genuine voluntary entry or structural coercion via prestige/geopolitical pressure?',
    'Analysis of bidding participation rates over time; interview data on domestic political pressure forcing bids; correlation between bidding and national status/diplomatic positioning; counterfactual: nations that chose not to bid and their justifications',
    'If voluntary: Rope perspective for bidding nations. If coercive: Tangled Rope or Snare (suppression increases, extraction increases).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bidding_coercion_mechanism, conceptual, 'Whether bidding process is voluntary or coerced').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(winter_olympics_venue_selection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woiv_tr_t0, winter_olympics_venue_selection, theater_ratio, 0, 0.52).
narrative_ontology:measurement(woiv_tr_t5, winter_olympics_venue_selection, theater_ratio, 5, 0.62).
narrative_ontology:measurement(woiv_tr_t10, winter_olympics_venue_selection, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(woiv_be_t0, winter_olympics_venue_selection, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(woiv_be_t5, winter_olympics_venue_selection, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(woiv_be_t10, winter_olympics_venue_selection, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(winter_olympics_venue_selection, resource_allocation).
narrative_ontology:affects_constraint(winter_olympics_venue_selection, olympic_infrastructure_debt).
narrative_ontology:affects_constraint(winter_olympics_venue_selection, climate_suitable_venue_scarcity).

% DUAL FORMULATION NOTE:
% Venue selection process is upstream of infrastructure construction and post-Olympic maintenance. Infrastructure debt is downstream of venue selection and inherits extractiveness through multiplier effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(winter_olympics_venue_selection, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
