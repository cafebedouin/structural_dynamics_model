% ============================================================================
% CONSTRAINT STORY: overfishing_north_atlantic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_overfishing_north_atlantic, []).

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
 *   constraint_id: overfishing_north_atlantic
 *   human_readable: Overfishing in the North Atlantic
 *   domain: environmental/economic/regulatory
 *
 * SUMMARY:
 *   Overfishing in the North Atlantic represents a classic extraction
 *   constraint masked by regulatory theater. For decades, industrial fishing
 *   fleets have harvested fish stocks at rates exceeding biological
 *   sustainability, concentrating wealth and employment in large corporations
 *   while externalizing ecosystem costs onto coastal communities, marine
 *   life, and future generations. The constraint operates through three
 *   mechanisms: (1) regulatory capture — quota-setting bodies are dominated
 *   by fishing interests, and quotas are routinely set above scientifically
 *   recommended levels; (2) suppression of alternatives — fuel subsidies make
 *   industrial fleet operations artificially cheap, and small-scale fishing
 *   is squeezed out through capital competition; (3) temporal asymmetry —
 *   extraction happens in real-time while ecosystem recovery is measured in
 *   generational timescales. The regulatory regime (NAFO for international
 *   waters, EU Common Fisheries Policy for EU waters) creates the appearance
 *   of conservation while enabling accelerated depletion. Stock assessments
 *   are published, quota meetings are held, compliance is monitored — the
 *   theater is substantial — but functional outcomes are grim: Atlantic cod
 *   stocks collapsed in the 1990s and remain depleted; megafauna (sharks,
 *   rays, large groundfish) are functionally extinct across much of the
 *   region; and the fishing industry itself now threatens collapse as
 *   catch-per-unit-effort approaches zero. The constraint exhibits snare
 *   dynamics from the powerless perspective (coastal communities trapped by
 *   economic dependency), tangled rope from moderate-scale operators (genuine
 *   coordination function shadowed by asymmetric extraction), rope from
 *   industrial corporations (arbitrage options abundant), and performative
 *   piton from the regulatory regime (conservation theater with minimal
 *   functional effect).
 *
 * KEY AGENTS:
 *   - Industrial Fishing Corporations: Primary beneficiary (institutional/arbitrage) — capture rents during depletion phase; can relocate to new fisheries or substitute species when stocks collapse
 *   - Coastal Fishing Communities: Primary victim (powerless/trapped) — economically dependent on fishing; lack alternative livelihoods; face income collapse as stocks decline
 *   - Marine Ecosystem: Secondary victim (powerless/trapped) — cannot exit; fish populations, breeding grounds, and trophic structure bear extraction cost; ecosystem recovery requires generational timescales
 *   - Mid-Scale Fleet Operators: Mixed (moderate/constrained) — benefit from coordination (quota allocation, technology, market access) while bearing asymmetric extraction (fuel costs, compliance burden, industrial competition)
 *   - NAFO/EU CFP Regulatory Regime: Institutional actor (institutional/arbitrage) — maintains performative conservation apparatus; sets quotas above sustainable levels due to political pressure
 *   - Environmental NGO Coalition: Organized victim (organized/constrained) — can exit through policy advocacy and market campaigns; faces suppression through regulatory delays and limited enforcement
 *   - Eco-Labeling and Certification Bodies: Organized coordinator (organized/constrained) — building scaffolding mechanisms (MSC certification) to transition from regulatory to market-based discipline
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political choice to extract as immutable biological constraint
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(overfishing_north_atlantic, 0.68).
domain_priors:suppression_score(overfishing_north_atlantic, 0.72).
domain_priors:theater_ratio(overfishing_north_atlantic, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(overfishing_north_atlantic, extractiveness, 0.68).
narrative_ontology:constraint_metric(overfishing_north_atlantic, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(overfishing_north_atlantic, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(overfishing_north_atlantic, snare).
narrative_ontology:human_readable(overfishing_north_atlantic, "Overfishing in the North Atlantic").
narrative_ontology:topic_domain(overfishing_north_atlantic, "environmental/economic/regulatory").

domain_priors:requires_active_enforcement(overfishing_north_atlantic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(overfishing_north_atlantic, large_industrial_fishing_fleets).
narrative_ontology:constraint_beneficiary(overfishing_north_atlantic, seafood_processing_corporations).
narrative_ontology:constraint_victim(overfishing_north_atlantic, small_scale_fishing_communities).
narrative_ontology:constraint_victim(overfishing_north_atlantic, marine_ecosystem_health).
narrative_ontology:constraint_victim(overfishing_north_atlantic, future_fish_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL FISHING COMMUNITY (SNARE) — Trapped by economic dependency on fishing, geographic isolation, and lack of alternative livelihoods. Faces maximum extraction as fleet capacity expands, catches decline, and subsidized industrial competition eliminates their margins. No meaningful exit option; bears full cost of resource collapse while beneficiaries capture rents during depletion phase.
constraint_indexing:constraint_classification(overfishing_north_atlantic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MARINE ECOSYSTEM (SNARE) — Cannot exit the extraction mechanism. Fish stocks, breeding grounds, and ecological stability bear the full cost of fleet capacity exceeding sustainable yield. Suppression operates through regulatory capture and knowledge asymmetry — ecosystem data is dispersed and slow; extraction happens in real time.
constraint_indexing:constraint_classification(overfishing_north_atlantic, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MID-SCALE FLEET OPERATOR (TANGLED ROPE) — Faces high costs to exit (capital equipment, crew livelihoods, debt obligations) but also benefits from fishery coordination: management systems allocate quota, technology enables catch efficiency, market access is secured. Extraction is asymmetric — must pay fuel and compliance costs while industrial competitors receive fuel subsidies — but genuine coordination function exists.
constraint_indexing:constraint_classification(overfishing_north_atlantic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL FISHING CORPORATION (ROPE) — Experiences the constraint as coordination: quota systems allocate fishing rights, processing infrastructure is shared, market access is coordinated. Net beneficiary through arbitrage — can relocate to different fisheries, substitute species, and use political influence to secure favorable quota allocation. Extraction runs toward this agent.
constraint_indexing:constraint_classification(overfishing_north_atlantic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY REGIME (PITON) — Quota-based management systems are largely performative: quota levels are set above sustainable yield due to political pressure; 'fishing down the food web' strategies circumvent species-specific limits; discard rates are unreported; and compliance monitoring is underfunded. The regulatory apparatus persists through institutional inertia (NAFO meetings, EU Common Fisheries Policy cycles) despite low functional effectiveness. Theater ratio reflects gap between announced conservation targets and realized extraction.
constraint_indexing:constraint_classification(overfishing_north_atlantic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL NGO COALITION (TANGLED ROPE) — Organized agents (Greenpeace, Oceanic Foundation, etc.) can exit the extraction mechanism through litigation, market campaigns, and policy advocacy. They have agency and coordination function — they work to restore ecosystem health — but face suppression through regulatory delays, industry lobbying, and limited enforcement resources. Constrained exit reflects that policy change is difficult but not impossible.
constraint_indexing:constraint_classification(overfishing_north_atlantic, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: MSC AND ECO-LABELING (SCAFFOLD) — Market-based certification and eco-labeling represent a temporary scaffolding mechanism: higher prices for certified sustainable catch incentivize managed practices, but certification is expensive (excludes small-scale operators), verifiable only post-hoc, and subject to regulatory capture (standards are weakened to maintain industry participation). Sunset logic: as consumer demand for certified sustainable seafood matures, and as blockchain traceability reduces certification cost, the scaffold can transition to genuine market discipline. Suppression is moderate because market mechanisms can escape regulatory capture.
constraint_indexing:constraint_classification(overfishing_north_atlantic, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some degree of fishing mortality is inherent to marine resource extraction: yields must be managed, depletion is a thermodynamic consequence of unregulated harvest, and catch limits are immutable biological constraints. This perspective risks naturalizing what is actually a political choice — the magnitude of suppression and extraction are consequences of governance structures, not laws of physics. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(overfishing_north_atlantic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(overfishing_north_atlantic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(overfishing_north_atlantic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(overfishing_north_atlantic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(overfishing_north_atlantic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(overfishing_north_atlantic, TR),
    TR >= 0.70.

:- end_tests(overfishing_north_atlantic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint concentrates benefits (corporate profits, employment in processing and retail) among large fishing entities while externalizing costs (stock collapse, community economic devastation, ecosystem degradation) onto powerless agents over long timescales. The extractiveness value reflects that the magnitude of rent captured by industrial operators exceeds the coordination services they provide — overfishing for short-term profit rather than sustainable yield management. Suppression (0.72): Very high. Multiple suppression mechanisms operate: (1) regulatory capture prevents quota reduction despite scientific evidence; (2) fuel subsidies artificially lower industrial fleet operating costs, preventing small-scale competitors from earning viable income; (3) alternative livelihood options are absent or require massive retraining investment; (4) knowledge asymmetry — ecosystem data is dispersed and slow while extraction happens in real-time; (5) enforcement is underfunded (patrol vessels are rare; port inspections are sporadic). Theater ratio (0.55): Moderate-high. The regulatory regime creates substantial performative activity — annual NAFO meetings, stock assessments, quota announcements, compliance monitoring frameworks — but the theater is not overwhelming because actual enforcement mechanisms (vessel tracking, landing inspections, scientific monitoring) do exist and generate some real constraint on extraction. The theater has increased over the interval as the gap between announced conservation goals and achieved outcomes has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how power structures generate divergent classifications. Same factual situation — declining fish stocks, industrial-scale harvesting, regulatory quotas — appears as snare (trapped victim), rope (beneficiary coordinator), piton (regulatory theater), tangled rope (moderate operator), scaffold (emerging alternatives), and false mountain (analytical naturalization) depending on structural position. The gap reveals the extraction mechanism: the beneficiary experiences coordination because they benefit from the constraint; the victim experiences extraction because they bear the cost; the analyst risks seeing inevitability because the extraction is dressed in conservation theater and biological language.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality flow is clear: extraction runs from powerless coastal communities and marine ecosystems toward institutional industrial corporations. The suppression mechanisms (lack of alternatives, regulatory capture, information asymmetry) trap victims in place while beneficiaries maintain arbitrage options. The beneficiary (industrial corporation) experiences low effective extraction because the constraint serves their interests; the victim (coastal community) experiences maximum effective extraction because the constraint has no exit option and extracts all rents available. The moderate operator (mid-scale fleet) experiences medium extraction because they benefit from coordination but bear asymmetric costs. The extraction accumulates over time (measuring extractiveness rising from 0.42 to 0.68) because quota levels remain above sustainable yield and fish populations decline, forcing higher effort (fuel, labor) to achieve the same catch, shifting rents steadily toward industrial operators with capital to absorb rising costs.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by mapping perspectives to power structures rather than seeking a single 'true' type. The snare classification is correct from the powerless perspective (coastal communities trapped by extraction). The rope classification is correct from the institutional beneficiary perspective (industrial corporations experiencing coordination). The piton classification is correct from the regulatory regime perspective (theater without function). The tangled rope is correct from the moderate perspective (mixed benefit and burden). The scaffold is correct from the organized coalition perspective (building alternatives). The mountain is a false summit from the analytical perspective — the analysis reveals that what appears as immutable biological law (we must fish or starve; fish stocks naturally decline) is actually a contingent outcome of regulatory capture and suppression mechanisms. The mandatrophy is fully resolved: each type is legitimate from its own structural context; no single type is 'the truth.' The framework's job is to make visible which structural contexts enable which classifications.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sustainable_yield_ambiguity,
    'What constitutes sustainable yield for multi-species fisheries with complex ecological interactions?',
    'Long-term ecosystem modeling; comparison of single-species vs ecosystem-based management outcomes; historical analysis of fishery collapses and recovery trajectories',
    'If sustainable yield is determinate and knowable: snare classification holds (extraction is a deliberate choice to exceed known limits). If unknowable: some extraction is necessary precaution (reclassifies as tangled_rope with coordination function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainable_yield_ambiguity, empirical, 'Determinacy of sustainable yield in complex marine ecosystems').

omega_variable(
    regulatory_capture_extent,
    'To what extent do catch quotas reflect biological science vs political-economic pressure from fishing interests?',
    'Comparison of scientifically recommended vs politically set quotas; analysis of lobbying expenditure correlation with quota decisions; longitudinal tracking of species status vs policy change',
    'If quotas are scientifically grounded: suppression is moderate (limits constrain but don''t eliminate extraction). If quotas are systematically inflated: suppression is primarily political theater (classification shifts to piton from regulatory perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_extent, empirical, 'Degree of regulatory capture in quota-setting processes').

omega_variable(
    iuu_fishing_quantification,
    'What is the actual volume of illegal, unreported, unrestricted (IUU) fishing, and how does it compare to reported catch?',
    'Satellite vessel monitoring; port inspection data; genetic analysis of seafood in retail markets; landings data cross-validation',
    'If IUU fishing represents <10% of total catch: regulatory suppression accounts for observed stock declines (snare classification holds). If IUU represents >30%: suppression is insufficient to explain decline, indicating ecosystem-driven degradation independent of regulation (reclassifies as mountain from ecosystem perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iuu_fishing_quantification, empirical, 'Magnitude of illegal fishing relative to reported landings').

omega_variable(
    ecosystem_tipping_point_proximity,
    'How close are North Atlantic fish stocks to ecosystem tipping points (regime shifts, food web collapse)?',
    'Stock assessment modeling with ecosystem feedback; paleoceanographic data on historical regime shifts; current biodiversity and trophic structure metrics',
    'If tipping points are distant: snare classification captures political choice to extract. If tipping points are imminent (<5 years): constraint reclassifies toward mountain (extraction becomes physically irreversible, shifting from political to thermodynamic immutability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ecosystem_tipping_point_proximity, empirical, 'Proximity to critical ecosystem collapse thresholds').

omega_variable(
    alternative_livelihood_feasibility,
    'Can displaced fishing communities transition to alternative economic activities at current scale without income loss?',
    'Cost-benefit analysis of retraining programs; regional economic capacity assessments; successful transition case studies; comparative wage analysis',
    'If transition is feasible and affordable: exit_options for coastal communities upgrade from trapped to constrained (snare classification softens). If transition is infeasible: trap is structural and inescapable (snare classification hardens).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_livelihood_feasibility, empirical, 'Feasibility of alternative livelihoods for fishing-dependent communities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(overfishing_north_atlantic, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(overfish_tr_t0, overfishing_north_atlantic, theater_ratio, 0, 0.35).
narrative_ontology:measurement(overfish_tr_t15, overfishing_north_atlantic, theater_ratio, 15, 0.48).
narrative_ontology:measurement(overfish_tr_t30, overfishing_north_atlantic, theater_ratio, 30, 0.55).
narrative_ontology:measurement(overfish_tr_t45, overfishing_north_atlantic, theater_ratio, 45, 0.6).

% Extraction over time
narrative_ontology:measurement(overfish_be_t0, overfishing_north_atlantic, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(overfish_be_t15, overfishing_north_atlantic, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(overfish_be_t30, overfishing_north_atlantic, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(overfish_be_t45, overfishing_north_atlantic, base_extractiveness, 45, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(overfishing_north_atlantic, resource_allocation).
narrative_ontology:boltzmann_floor_override(overfishing_north_atlantic, 0.18).
narrative_ontology:affects_constraint(overfishing_north_atlantic, coastal_community_economic_collapse).
narrative_ontology:affects_constraint(overfishing_north_atlantic, marine_biodiversity_loss_north_atlantic).
narrative_ontology:affects_constraint(overfishing_north_atlantic, regulatory_capture_fisheries_management).

% DUAL FORMULATION NOTE:
% Overfishing North Atlantic decomposes into three related constraints: (1) the extraction mechanism (quota-setting above sustainable yield), (2) the economic collapse of coastal communities (geographic isolation + dependency), and (3) the ecosystem degradation (stock depletion + food web restructuring). Each has different ε values and different beneficiary/victim mappings. This story focuses on the extraction constraint; downstream stories address community and ecosystem impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(overfishing_north_atlantic, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
