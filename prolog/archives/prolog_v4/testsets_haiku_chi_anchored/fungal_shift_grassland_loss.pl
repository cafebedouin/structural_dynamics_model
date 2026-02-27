% ============================================================================
% CONSTRAINT STORY: fungal_shift_grassland_loss
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fungal_shift_grassland_loss, []).

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
 *   constraint_id: fungal_shift_grassland_loss
 *   human_readable: Ecological Transition from Grassland to Scrub via Fungal Network Disruption
 *   domain: ecology/climate/mycology
 *
 * SUMMARY:
 *   Warming winters in mountain and subalpine grasslands have reduced the
 *   stability of winter snowpack, creating a shift in soil freeze-thaw
 *   dynamics. Traditional winter snow insulates soil and supports mycorrhizal
 *   fungi during dormancy. Without stable snowpack, soil experiences greater
 *   temperature fluctuations, repeated freeze-thaw cycles, and longer periods
 *   of subfreezing stress. Mycorrhizal fungal networks — which form obligate
 *   symbioses with grassland plant roots and are essential for nutrient
 *   uptake and water stress tolerance — become destabilized. As fungal
 *   networks collapse, grassland species lose competitive advantage against
 *   scrub and woody species, which are thermally favored by longer growing
 *   seasons and have lower mycorrhizal dependency. This ecological transition
 *   exhibits all hallmarks of a tangled rope constraint: there is a genuine
 *   coordination function (mycorrhizal fungi coordinating nutrient flow and
 *   grass competitiveness), but the disruption of this function creates
 *   asymmetric extraction — pastoral communities, grassland-dependent
 *   wildlife, and the fungal network integrity itself bear costs, while
 *   beneficiaries (scrub species, climate-beneficiary plants, some
 *   restoration practitioners) gain from the transition. The constraint
 *   operates across generational timescales but has biographical impact on
 *   human livelihoods. Land management agencies attempt performative
 *   interventions (grass seeding, prescribed burns) that do little to restore
 *   the underlying fungal networks, creating a piton characteristic —
 *   maintenance of outdated management ideologies despite low efficacy.
 *
 * KEY AGENTS:
 *   - Grassland-Dependent Herbivores: Primary victims (powerless/trapped) — alpine and subalpine fauna lose habitat as fungal networks collapse
 *   - Pastoral Communities: Primary victims (moderate/constrained) — herders dependent on stable grassland forage face degraded carrying capacity
 *   - Mycorrhizal Network Integrity: Tertiary victim (powerless/trapped) — fungal networks disrupted by freeze-thaw instability; abstract collective with no exit
 *   - Scrub-Adapted Plants: Primary beneficiaries (institutional/arbitrage) — sagebrush, juniper, woody invasives expand as fungal collapse removes grass competition
 *   - Ecological Restoration Practitioners: Secondary beneficiary (organized/constrained) — extract funding and research opportunities from restoration crisis
 *   - Land Management Agencies: Institutional actor (institutional/arbitrage) — maintain performative grassland management protocols despite declining efficacy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing socio-ecological entanglement as pure climate physics constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fungal_shift_grassland_loss, 0.38).
domain_priors:suppression_score(fungal_shift_grassland_loss, 0.52).
domain_priors:theater_ratio(fungal_shift_grassland_loss, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, extractiveness, 0.38).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(fungal_shift_grassland_loss, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fungal_shift_grassland_loss, tangled_rope).
narrative_ontology:human_readable(fungal_shift_grassland_loss, "Ecological Transition from Grassland to Scrub via Fungal Network Disruption").
narrative_ontology:topic_domain(fungal_shift_grassland_loss, "ecology/climate/mycology").

domain_priors:requires_active_enforcement(fungal_shift_grassland_loss).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fungal_shift_grassland_loss, scrub_adapted_plants).
narrative_ontology:constraint_beneficiary(fungal_shift_grassland_loss, warming_climate_beneficiaries).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, grassland_dependent_species).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, mycorrhizal_network_integrity).
narrative_ontology:constraint_victim(fungal_shift_grassland_loss, pastoral_livelihoods).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GRASSLAND-DEPENDENT HERBIVORES (SNARE) — Alpine and subalpine grazers (bighorn sheep, mountain goats, grasshoppers) lose habitat as fungal networks collapse under freeze-thaw instability. Trapped by geography and diet specificity. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.51.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: PASTORAL COMMUNITIES (SNARE) — Herders and ranchers dependent on stable grassland forage face degraded carrying capacity as scrub encroaches. Constrained by capital investment in grazing rights and livestock. Cannot rapidly pivot to alternative livelihoods. d≈0.78, f(d)≈1.08, σ=0.9 → χ≈0.42.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ECOLOGICAL RESTORATION PRACTITIONERS (TANGLED ROPE) — Conservation organizations and land managers benefit from restoration contracts and research funding generated by the crisis, but are constrained by the scale of ecosystem shift and limitations on active intervention efficacy. Coordination function: mobilizing knowledge to slow transition. Asymmetric extraction: funders extract accountability; practitioners extract funding. d≈0.55, f(d)≈0.75, σ=0.9 → χ≈0.31.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: CLIMATE-BENEFICIARY PLANT SPECIES (ROPE) — Scrub and shrub species (sagebrush, juniper, woody invasives) benefit from longer growing seasons and reduced freeze-thaw stress. The fungal network disruption removes a primary competitor (grass). Experiences constraint as coordination: fungal collapse is selection mechanism enabling their expansion. d≈0.08, f(d)≈-0.07, σ=0.9 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LAND MANAGEMENT AGENCIES (PITON) — Traditional grassland management protocols (grazing permits, prescribed burns, grass seeding) increasingly performative as fungal networks collapse for reasons beyond management control. Agencies maintain rituals of grassland restoration despite low efficacy. theater_ratio=0.48 borderline; rising as interventions fail. Institutional inertia maintains outdated management plans. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.14.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the perspective of climate physics, the loss of stable snowpack and consequent soil freeze-thaw instability is a direct consequence of warming — a constraint imposed by atmospheric thermodynamics. No degree of freedom to maintain grassland ecosystems if the underlying thermal regime shifts permanently. However, the structural data (ε=0.38, suppression=0.52, theater=0.48) suggests this is not a pure natural law but a socio-ecological entanglement where human land management choices and species range shifts amplify the underlying climate constraint.
constraint_indexing:constraint_classification(fungal_shift_grassland_loss, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fungal_shift_grassland_loss_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fungal_shift_grassland_loss, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(fungal_shift_grassland_loss, TR),
    TR >= 0.70.

:- end_tests(fungal_shift_grassland_loss_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The fungal network disruption imposes costs on pastoral livelihoods and grassland-dependent species, but extraction is not as severe as a pure snare because (a) ecological transitions are not absolute — some grassland pockets persist, and (b) some human communities and species benefit. The constraint is asymmetric: some actors experience it as devastating (pastoral communities), others as opportunistic (scrub species). Suppression (0.52): Moderate-high. Pastoral communities face significant barriers to alternative livelihoods and face pressure to maintain grazing in degraded grasslands rather than exit. Grassland-dependent species cannot exit geography. However, suppression is not maximal because land management agencies do attempt interventions (though ineffective), and some pasture rotation/resting practices exist. Theater ratio (0.48): Low-moderate. Agencies' interventions (seeding, burning) have declining efficacy as the underlying fungal networks fail, but the ritual is not yet wholly performative — some grassland patches do respond to management, and the protocols have historical legitimacy. Theater is rising as efficacy declines (measurements show progression from 0.35 to 0.48). This trajectory suggests potential piton evolution — if theater rises above 0.70, the constraint will reclassify.
 *
 * PERSPECTIVAL GAP:
 *   Pastoral communities and grassland fauna experience this as a snare — they are trapped by geography, capital investment, and diet specificity, with no escape. Ecological restoration practitioners experience it as tangled rope — they both benefit (funding, purpose) and are constrained (limited efficacy, scale of change). Scrub species and climate beneficiaries experience it as rope — a coordination mechanism that benefits them. Land management agencies experience it as piton — they maintain outdated grassland-restoration protocols despite increasingly poor efficacy. The analytical observer risks seeing a pure mountain (natural law of climate physics), but the structural data reveals that human land management choices (grazing intensity, fire suppression, monoculture seeding) have historically amplified fungal dependency and reduced ecosystem resilience, making the transition a socio-ecological entanglement rather than pure climate determination.
 *
 * DIRECTIONALITY LOGIC:
 *   Grassland-dependent herbivores: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction. Pastoral communities: Victim + constrained → d≈0.78, f(d)≈1.08. High extraction but not maximal (have some agency in herd management choices). Restoration practitioners: Beneficiary + constrained → d≈0.55, f(d)≈0.75. Mixed: extract funding but constrained by limited efficacy. Scrub species: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.07. Net beneficiary; experience constraint as enabling their expansion. Land management agencies: Institutional + arbitrage → d≈0.35, f(d)≈0.32. Low-moderate; agency maintains bureaucratic function even as efficacy declines (piton signature). Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain classification is perspectival; the engine's false summit detector will flag this as a risk of naturalizing a contingent socio-ecological arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the fungal network disruption creates genuine coordination loss (mycorrhizal fungi no longer coordinate nutrient/water flows effectively) combined with asymmetric extraction (costs borne by pastoral communities, fungal networks, grassland fauna; benefits accrued to scrub species and some restoration practitioners). The constraint is tangled rope, not pure snare, because: (1) there is a real coordination function being disrupted (mycorrhizal mutualism), (2) beneficiaries exist and depend on the constraint's effects (scrub species, climate-beneficiary plants), and (3) restoration practitioners exercise partial agency in managing the transition. If the constraint were pure extraction (snare), there would be no coordination function to disrupt — the victims would simply lose. The false summit risk (analytical observer seeing a mountain of climate physics) is resolved by noting that the constraint's strength (ε=0.38, suppression=0.52) reflects human agency choices in land management and livestock grazing intensity, not pure climate determination. Climate change is the proximal driver, but land management practices are the structural condition enabling the fungal networks' vulnerability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fungal_recovery_threshold,
    'Below what soil temperature range do mycorrhizal fungi undergo permanent network collapse rather than seasonal dormancy?',
    'Laboratory soil incubation studies; field surveys of fungal sporocarp production and hyphal biomass across elevation gradients and winter severity gradients',
    'If threshold is 2-3°C above current regional minima: near-term recovery possible with local warming stabilization. If threshold is 5°C+ below current minima: grassland-to-scrub transition is quasi-irreversible at regional scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fungal_recovery_threshold, empirical, 'Thermal threshold for permanent mycorrhizal network collapse').

omega_variable(
    grass_scrub_competitive_balance,
    'Is the grass-to-scrub transition driven primarily by fungal collapse (removing grass competitiveness) or by thermal-growing-season expansion favoring C3 scrub species?',
    'Experimental plots with fungal inoculant restoration in warmed conditions; comparison of scrub expansion rates in fungal-intact vs fungal-disrupted soils under identical thermal regimes',
    'If fungal collapse is primary driver: targeted mycorrhizal restoration could partially reverse transition. If scrub thermal advantage is primary: restoration efficacy is capped regardless of fungal status.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(grass_scrub_competitive_balance, empirical, 'Whether transition is fungal-driven or thermally-driven').

omega_variable(
    pastoral_economic_pivotability,
    'Can pastoral communities economically sustain scrub-adapted livestock (goats, camels) or transition to non-grazing livelihoods faster than regional grassland loss accelerates?',
    'Economic feasibility studies; tracking of livestock transitions and livelihood diversification in affected regions; comparison with historical pastoral adaptations to drought',
    'If pivot feasible within 1-2 decades: snare classification for pastoral communities is temporary. If pivot requires 3+ decades: snare is structural and long-term.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(pastoral_economic_pivotability, empirical, 'Whether pastoral communities can economically pivot to scrub-adapted systems').

omega_variable(
    fungal_network_ecosystem_coupling,
    'How tightly coupled are regional mycorrhizal networks — do fungi function as a single ''wood-wide web'' such that local collapse cascades, or do they function as independent local communities with limited inter-site transfer?',
    'Fungal DNA sequencing and network analysis; modeling of hyphal connectivity and nutrient transfer between sites; manipulation experiments isolating fungal communities',
    'If tightly coupled: cascading collapse across the region; transition is rapid and region-wide. If loosely coupled: collapse is patchy; pockets of fungal integrity could persist and enable grassland refugia.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fungal_network_ecosystem_coupling, empirical, 'Spatial coupling of mycorrhizal networks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fungal_shift_grassland_loss, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fungal_tr_t0, fungal_shift_grassland_loss, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fungal_tr_t20, fungal_shift_grassland_loss, theater_ratio, 20, 0.42).
narrative_ontology:measurement(fungal_tr_t40, fungal_shift_grassland_loss, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(fungal_be_t0, fungal_shift_grassland_loss, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(fungal_be_t20, fungal_shift_grassland_loss, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(fungal_be_t40, fungal_shift_grassland_loss, base_extractiveness, 40, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fungal_shift_grassland_loss, resource_allocation).
narrative_ontology:affects_constraint(fungal_shift_grassland_loss, alpine_grassland_carbon_sink_loss).
narrative_ontology:affects_constraint(fungal_shift_grassland_loss, pastoral_drought_resilience_degradation).

% DUAL FORMULATION NOTE:
% The fungal shift is downstream of climate warming (reduced stable snowpack) but represents a distinct constraint on ecological-livelihood coupling. Upstream climate constraints (temperature regime shifts, precipitation seasonality) are separate structural phenomena; the fungal network disruption is the mechanism by which climate warming translates into grassland-to-scrub transition and livelihood extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fungal_shift_grassland_loss, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
