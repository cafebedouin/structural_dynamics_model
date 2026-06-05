% ============================================================================
% CONSTRAINT STORY: polar_bear_biobanking
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_polar_bear_biobanking, []).

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
 *   constraint_id: polar_bear_biobanking
 *   human_readable: Polar Bear Genetic Biobanking as a Climate Change Hedge
 *   domain: technological/political
 *
 * SUMMARY:
 *   Polar bear biobanking presents a policy mechanism that hybridizes
 *   coordination and extraction around climate change uncertainty. On the
 *   surface, it is a rational hedge: if climate change eliminates wild
 *   populations despite mitigation efforts, preserved genomes enable
 *   restoration. Structurally, however, the constraint exhibits all the
 *   hallmarks of tangled rope: genuine coordination benefits (standardized
 *   genetic infrastructure enables distributed research, technology transfer,
 *   institutional collaboration) coexist with asymmetric extraction (the
 *   constraint channels resources toward de-extinction research while
 *   implicitly deferring climate mitigation urgency, and suppresses
 *   alternative adaptive strategies like habitat protection and emissions
 *   reduction). The constraint's extractiveness has risen from ~0.28 (early
 *   biobanking as purely complementary to mitigation, 2015-2018) to 0.52
 *   (current state where biobanking narratives are increasingly deployed to
 *   justify inaction on climate policy). Theater ratio has risen in parallel
 *   (0.42 → 0.68), indicating that the biobanking program's primary function
 *   is increasingly rhetorical (appearing as climate action) rather than
 *   instrumental (actually advancing extinction prevention through either
 *   mitigation or genetic rescue). The decomposition reveals that 'polar bear
 *   biobanking' conflates three structurally distinct constraints: (1) the
 *   coordination problem of distributed genetic sequencing (rope), (2) the
 *   extractive substitution of biobanking for habitat protection (snare), and
 *   (3) the temporal scaffolding of de-extinction as a transitional
 *   technology (scaffold). The analytical observer risks naturalizing policy
 *   choice as physical inevitability — the 'mountain' framing that treats
 *   species extinction as an unchangeable law of climate physics rather than
 *   an outcome of political decisions about emissions.
 *
 * KEY AGENTS:
 *   - De-Extinction Research Institutions: Primary beneficiary (institutional/arbitrage) — capture funding, publications, infrastructure contracts; can easily pivot to other species
 *   - Genomic Technology Firms: Secondary beneficiary (institutional/arbitrage) — gain from biobanking contracts, sequencing services, gene synthesis capabilities
 *   - Wild Polar Bear Populations: Primary victim (powerless/trapped) — cannot exit climate-driven habitat loss; biobanking constraint defers protection investment
 *   - Arctic Coastal Communities: Co-victim (powerless/trapped) — depend on living populations; face extraction through narrative substitution of genetic rescue for habitat preservation
 *   - Climate Policy Advocates: Organized observer (organized/constrained) — see biobanking as temporary scaffold toward genuine mitigation; constrained by policy inertia
 *   - Conservation Organizations: Mixed position (organized/constrained) — benefit from biobanking-linked funding but constrained from prioritizing habitat work by research infrastructure incentives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks misclassifying policy choice as natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(polar_bear_biobanking, 0.52).
domain_priors:suppression_score(polar_bear_biobanking, 0.62).
domain_priors:theater_ratio(polar_bear_biobanking, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(polar_bear_biobanking, extractiveness, 0.52).
narrative_ontology:constraint_metric(polar_bear_biobanking, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(polar_bear_biobanking, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(polar_bear_biobanking, tangled_rope).
narrative_ontology:human_readable(polar_bear_biobanking, "Polar Bear Genetic Biobanking as a Climate Change Hedge").
narrative_ontology:topic_domain(polar_bear_biobanking, "technological/political").

domain_priors:requires_active_enforcement(polar_bear_biobanking).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, de_extinction_research_institutions).
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, conservation_funding_agencies).
narrative_ontology:constraint_beneficiary(polar_bear_biobanking, genomic_technology_firms).
narrative_ontology:constraint_victim(polar_bear_biobanking, wild_polar_bear_populations).
narrative_ontology:constraint_victim(polar_bear_biobanking, arctic_ecosystem_integrity).
narrative_ontology:constraint_victim(polar_bear_biobanking, climate_mitigation_urgency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WILD POLAR BEAR POPULATIONS (SNARE) — Bears cannot opt out of habitat loss driven by climate change. The biobanking constraint operates as a form of coercive substitution: the implicit message that genetic preservation suffices replaces the urgent need for habitat protection. The living population bears the extraction (continued climate inaction justified by biobank insurance) while research institutions capture the coordination benefits.
constraint_indexing:constraint_classification(polar_bear_biobanking, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ARCTIC COASTAL COMMUNITIES (SNARE) — Indigenous peoples and subsistence hunters have no exit from the constraint. They depend on living polar bear populations for food security and cultural continuity. The biobanking narrative implicitly deprioritizes habitat protection in favor of technological rescue, while communities bear the extraction cost of accelerated species collapse.
constraint_indexing:constraint_classification(polar_bear_biobanking, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONSERVATION ORGANIZATIONS (TANGLED ROPE) — NGOs see both coordination and extraction. Biobanking provides funding leverage ('future-proof the species') and generates publicity for conservation work. But the constraint also extracts: resources flow to genetic infrastructure rather than immediate habitat protection and emissions reduction. Exit is constrained by dependence on research funding that incentivizes de-extinction narratives over direct habitat work.
constraint_indexing:constraint_classification(polar_bear_biobanking, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DE-EXTINCTION RESEARCH INSTITUTIONS (ROPE) — Pure beneficiary with full arbitrage (can redirect resources to other species if polar bears cease to be funding-viable). The biobank creates coordination: a standardized genetic resource enables distributed research networks. No significant suppression of alternatives; institutions can exit if funding dries up. They experience the constraint as genuinely cooperative infrastructure.
constraint_indexing:constraint_classification(polar_bear_biobanking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GENOMIC TECHNOLOGY FIRMS (ROPE) — Benefit from standardized biobanking infrastructure contracts, sequencing services, and gene synthesis capabilities. Full arbitrage: if polar bears are no longer a market, technologies transfer to other de-extinction or conservation genomics projects. Pure coordination benefit with minimal coercion of the firm itself.
constraint_indexing:constraint_classification(polar_bear_biobanking, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CLIMATE POLICY ADVOCATES (SCAFFOLD) — See biobanking as a temporal scaffold: acceptable as short-term hedge while genuine emissions reductions scale up. The constraint has a sunset if atmospheric CO2 stabilization and Arctic warming slowdown materialize within 30 years. Theater is moderate (biobanking appears as urgent climate action while being decoupled from actual mitigation). Exit is constrained by political economy of climate policy, but advocates hope the scaffold eventually collapses as the underlying need (mitigation) makes genetic rescue unnecessary.
constraint_indexing:constraint_classification(polar_bear_biobanking, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: THE DE-EXTINCTION NARRATIVE AS PITON (CIVILIZATIONAL VIEW) — From a long timescale, polar bear biobanking is largely theatrical. De-extinction technology requires stable geopolitical order, functioning laboratories, power infrastructure, and institutional continuity across centuries — all fragile under continued climate chaos. The biobank's primary function has degraded to justifying inaction on climate mitigation. It persists through narrative inertia ('we're doing something'), not because the technical approach would actually work as advertised. Theater ratio is high (73% of biobanking's rhetorical work is 'we have a backup plan'); functional restoration capacity is low.
constraint_indexing:constraint_classification(polar_bear_biobanking, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 8: PHYSICAL/ECOLOGICAL LIMITS (MOUNTAIN — FALSE SUMMIT) — Some might frame species extinction as an immutable natural consequence of rapid climate change — a force beyond human control, therefore rendering all biobanking as merely accepting inevitable loss. However, the structural data contradicts this. Polar bear extinction is not a natural law; it is a consequence of anthropogenic emissions that are technologically stoppable. The 'mountain' framing naturalizes a contingent policy choice (continued emissions) as an inescapable fact. The engine will flag this as a false summit: the constraint is tangled_rope/snare/scaffold (policy-artifact), not mountain (natural law).
constraint_indexing:constraint_classification(polar_bear_biobanking, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(polar_bear_biobanking_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(polar_bear_biobanking, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(polar_bear_biobanking, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(polar_bear_biobanking, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(polar_bear_biobanking, TR),
    TR >= 0.70.

:- end_tests(polar_bear_biobanking_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. The constraint extracts resources from climate mitigation budgets and defers urgency on habitat protection, but it is not pure extraction — genuine coordination benefits exist (standardized genetic infrastructure, technology transfer, institutional collaboration). The rising trajectory (0.28 → 0.52 over 10 years) reflects how biobanking narratives have increasingly displaced mitigation framing in climate policy discourse. Suppression (0.62): High. Significant structural barriers prevent alternatives from being prioritized: (a) institutional inertia around de-extinction funding streams, (b) psychological license effect (biobank provides sense of security reducing political urgency), (c) publication/prestige incentives in research institutions favoring genetic rescue over habitat work, (d) geopolitical fragmentation preventing coordinated Arctic conservation. Theater ratio (0.68): High and rising. Traditional biobanking rhetoric frames genetic preservation as urgent climate action while decoupling from actual emissions reduction. The constraint's performative content has grown as climate policy has stalled — biobanking has become a primary mechanism for appearing to address species loss without addressing underlying drivers. The theater arises from the gap between the operational scope (laboratory-scale genetic work) and the claimed scope (preventing species extinction through climate adaptation). From a civilizational timescale, the theater is even more evident: for de-extinction to work requires stable geopolitical order, functioning institutions, and power infrastructure across centuries — all of which are at severe risk under continued climate chaos. The functional restoration capacity is therefore very low even as the rhetorical utility is very high.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival divide. Research institutions (beneficiaries with arbitrage) experience pure coordination: biobanking enables distributed networks, standardized samples, technology transfer, open research. They see the constraint as rope. Wild polar bears and Arctic communities (powerless/trapped) experience pure extraction: the biobanking narrative defers habitat protection and justifies climate inaction ('we have a backup plan'). They see the constraint as snare. Conservation organizations occupy an intermediate position (tangled rope): they benefit from biobanking-linked funding and infrastructure but are also constrained by the same incentives from pursuing more urgent habitat work. The scaffold perspective (climate advocates) sees biobanking as acceptable only if temporary — a bridge technology valid only until genuine mitigation scales up. The piton perspective (civilizational view) reveals the constraint as theater: the de-extinction narrative persists through inertia, not because the technical approach would actually work given geopolitical fragility and infrastructure continuity risks. The analytical observer's potential mountain framing ('extinction is inevitable under climate change') is a false summit: it naturalizes the political choice to maintain emissions as though it were a physical law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and structural position. De-extinction institutions have low d (full beneficiaries with arbitrage exit): they experience the constraint as positive coordination. They can walk away if funding evaporates. Wild bears and Arctic communities have high d (full victims with trapped exit): they experience maximum extraction because they cannot exit the habitat loss that the biobanking constraint defers addressing. Conservation organizations have intermediate d (mixed beneficiary from funding, victim from constrained exit): they benefit from biobanking money but cannot easily exit because their institutional model depends on research funding that incentivizes de-extinction narratives. The directionality computation automatically derives these relationships from the declared beneficiaries/victims and exit options, producing the sigmoid f(d) values that scale extractiveness. The constraint's suppression (0.62) is unscaled — it reflects the actual structural barriers to habitat alternatives, not a context-dependent quantity.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by clarifying that 'polar bear biobanking' is not a single structural constraint but a presheaf covering three distinct problems: (1) genetic coordination infrastructure (rope: low extraction, genuine coordination benefit), (2) habitat protection deferral (snare: high extraction, no coordination), and (3) temporal scaffolding toward mitigation (scaffold: moderate extraction, sunset clause). The unified policy narrative ('biobanking as climate hedge') conflates these three types. The mandatrophy is resolved by showing that the high theater ratio (0.68) and rising extractiveness (0.28 → 0.52) reveal that the snare component has grown — the constraint's primary function is increasingly to justify inaction on mitigation, not to advance genetic rescue or habitat protection. The analytical observer's temptation to classify the constraint as a mountain ('species extinction is inevitable') is a false summit: the classification catches the author naturalizing contingent policy choices (continued emissions) as physical laws. The proper classification at the analytical context is tangled_rope or piton (depending on whether you emphasize the residual coordination benefits or the dominant theater), not mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    de_extinction_technical_feasibility,
    'Can preserved polar bear genomes actually be used to recreate viable populations, or is de-extinction a technological fantasy that justifies inaction on climate mitigation?',
    'Comparative assessment of technical barriers (genome completeness, epigenetic knowledge, surrogate species availability, sustained laboratory infrastructure) against timescale of climate emergency. Evaluate feasibility of other de-extinction projects (mammoth, dodo) as proxies for polar bear technical plausibility.',
    'If feasible: biobanking is legitimate hedge (scaffold/rope perspective strengthens). If not feasible: biobanking is pure theater masking inaction (piton/snare perspective dominates, extractiveness rises to 0.70+).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(de_extinction_technical_feasibility, empirical, 'Technical feasibility of de-extinction from preserved genomes').

omega_variable(
    substitution_effect_magnitude,
    'Does biobanking funding actually substitute for climate mitigation funding, or are the resource pools sufficiently separate that biobanking adds without displacing mitigation?',
    'Analysis of funding flows: conservation budgets before/after biobanking announcements; opportunity cost accounting; political economy of climate vs conservation funding allocation in major funding bodies (NSF, EU Horizon, IPCC-linked agencies).',
    'If high substitution (>0.6): suppression rises (biobanking suppresses alternatives like emissions reduction). If low substitution: suppression falls, constraint becomes closer to pure rope. Mandatrophy resolves: if substitution high, the ''this is a hedge'' narrative is extractive (snare); if low, it''s genuine coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_effect_magnitude, empirical, 'Degree to which biobanking funding displaces climate mitigation').

omega_variable(
    psychological_license_effect,
    'Does the existence of a biobanking program reduce public and political urgency around climate mitigation by providing a sense of ''backup plan'' security?',
    'Opinion surveys and content analysis of policy discourse before/after biobanking announcements; correlation between biobank publicity and climate policy momentum; comparative analysis of public climate concern in regions with vs without de-extinction narratives.',
    'If strong license effect: suppression of climate alternatives increases, extractiveness rises to 0.65+. The constraint becomes more purely extractive (snare) because its primary function is rhetorical deferent of mitigation. If weak: the constraint remains tangled rope with genuine coordination alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psychological_license_effect, empirical, 'Whether biobanking reduces urgency for climate mitigation').

omega_variable(
    arctic_geopolitical_instability,
    'Given Arctic geopolitical tensions and climate-driven resource competition, is biobank infrastructure (distributed labs, sampling rights, genetic data) likely to survive sustained to enable de-extinction in the future?',
    'Assessment of geopolitical fragility: Arctic territorial disputes, lab location vulnerability to conflict, institutional continuity risk, data sovereignty conflicts over polar bear genomes (which nations own genetic material from bears in their waters?), and historical precedent for long-term scientific infrastructure survival across regime change.',
    'If infrastructure highly fragile: biobanking''s functional credibility as a hedge collapses. Extractiveness rises (becomes pure theater/snare: 0.70+). If robust: biobanking retains some genuine hedge function (rope/scaffold perspectives stay viable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arctic_geopolitical_instability, conceptual, 'Whether biobank infrastructure can survive Arctic geopolitical instability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(polar_bear_biobanking, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(polbear_tr_t0, polar_bear_biobanking, theater_ratio, 0, 0.42).
narrative_ontology:measurement(polbear_tr_t5, polar_bear_biobanking, theater_ratio, 5, 0.55).
narrative_ontology:measurement(polbear_tr_t10, polar_bear_biobanking, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(polbear_be_t0, polar_bear_biobanking, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(polbear_be_t5, polar_bear_biobanking, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(polbear_be_t10, polar_bear_biobanking, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(polar_bear_biobanking, information_standard).
narrative_ontology:affects_constraint(polar_bear_biobanking, arctic_climate_mitigation_deferral).
narrative_ontology:affects_constraint(polar_bear_biobanking, species_rescue_technology_hype).

% DUAL FORMULATION NOTE:
% Polar bear biobanking decomposes into three structurally distinct constraints: (1) genetic_coordination_infrastructure (rope: ε≈0.15), (2) habitat_protection_substitution (snare: ε≈0.68), and (3) climate_mitigation_temporal_scaffolding (scaffold: ε≈0.35). The unified story presents the tangled_rope aggregate (ε≈0.52) as a working policy compromise, while the network links show upstream constraints (climate mitigation deferral) and downstream constraints (technology hype cycles). The bifurcation into components becomes analytically necessary when biobanking narratives begin to dominate climate policy discourse (rising theater ratio indicates the coordinate and extractive components are decoupling).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(polar_bear_biobanking, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
