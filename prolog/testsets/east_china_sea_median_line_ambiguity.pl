% ============================================================================
% CONSTRAINT STORY: east_china_sea_median_line_ambiguity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_east_china_sea_median_line_ambiguity, []).

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
 *   constraint_id: east_china_sea_median_line_ambiguity
 *   human_readable: East China Sea Median Line Ambiguity
 *   domain: geopolitical/maritime_law/resource_extraction
 *
 * SUMMARY:
 *   The East China Sea median line ambiguity is a geopolitical constraint
 *   that emerges from the intersection of maritime law, resource scarcity,
 *   and power asymmetry. UNCLOS (United Nations Convention on the Law of the
 *   Sea) establishes the median line principle as the default boundary for
 *   exclusive economic zones (EEZ) in overlapping maritime claims. However,
 *   both China and Japan contest each other's baseline definitions—the
 *   geographic starting points from which the median line is calculated.
 *   China claims an outer continental shelf (OCS) boundary that extends the
 *   Chinese baseline far into the ECS, shifting the median line southward and
 *   granting China access to hydrocarbon deposits and fishing zones that
 *   Japan claims under a median line calculated from different baselines.
 *   Japan adheres to a baseline closer to its actual low-water mark
 *   coastline, pushing the median line northward. This structural
 *   ambiguity—the lack of agreement on baselines—creates a legal deadlock
 *   that both states exploit for resource extraction and geopolitical
 *   advantage. The constraint exhibits all six classification types from
 *   different perspectives: for powerless fishing communities, it is a snare
 *   (trapped extraction with no exit); for both major states, it is a tangled
 *   rope (genuine coordination function via UNCLOS exists alongside
 *   asymmetric extraction through baseline manipulation and coast guard
 *   enforcement); for international law institutions, it is a scaffold with
 *   sunset logic (arbitration and law-based resolution offer paths to
 *   stability but these pathways can be rendered irrelevant by military
 *   dominance); for the abstract UNCLOS principle, it is a piton (the rule
 *   persists through citation ritual but functional coordination has
 *   degraded); and from a civilizational analytical view, it risks appearing
 *   as a natural law (geometric ambiguity in overlapping zones) when in fact
 *   it is an instrumentalized geopolitical extraction mechanism. The
 *   constraint's theater ratio has increased over the past decade as legal
 *   argumentation has become increasingly sophisticated while actual resource
 *   extraction and military enforcement have intensified—the performative
 *   framing (invocation of UNCLOS, arbitration rhetoric) now substantially
 *   obscures the underlying extraction dynamics.
 *
 * KEY AGENTS:
 *   - China (PRC State): Powerful beneficiary (powerful/constrained) — extracts hydrocarbon access and fishing zone expansion through coast guard enforcement and baseline assertions; constrained by US alliance with Japan and UNCLOS legitimacy costs but increasingly dominant militarily
 *   - Japan (State): Powerful beneficiary (powerful/constrained) — extracts median-line EEZ rights through legal assertion and limited coast guard presence; constrained by alliance obligations and relative military disadvantage against China
 *   - Regional Fishing Communities: Primary victims (powerless/trapped) — small-scale fishers from Taiwan, Philippines, Vietnam, and Southeast Asia face seizure and violence with no recourse; cannot exit territorial waters due to economic necessity
 *   - Maritime Law Institutions: Organized institutional actors (organized/mobile) — UNCLOS signatories, arbitration frameworks (ITLOS, ICJ), and legal experts providing scaffolding for boundary resolution; maintain exit options through continued arbitration processes
 *   - UNCLOS Median Line Principle: Institutional abstraction (institutional/mobile) — the mathematical rule exists independent of enforcement; persists through citation but function degrades as enforcement diverges from principle
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the ambiguity as an inherent geometric property when it is actually instrumentalized for geopolitical extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(east_china_sea_median_line_ambiguity, 0.58).
domain_priors:suppression_score(east_china_sea_median_line_ambiguity, 0.72).
domain_priors:theater_ratio(east_china_sea_median_line_ambiguity, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(east_china_sea_median_line_ambiguity, extractiveness, 0.58).
narrative_ontology:constraint_metric(east_china_sea_median_line_ambiguity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(east_china_sea_median_line_ambiguity, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(east_china_sea_median_line_ambiguity, tangled_rope).
narrative_ontology:human_readable(east_china_sea_median_line_ambiguity, "East China Sea Median Line Ambiguity").
narrative_ontology:topic_domain(east_china_sea_median_line_ambiguity, "geopolitical/maritime_law/resource_extraction").

domain_priors:requires_active_enforcement(east_china_sea_median_line_ambiguity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(east_china_sea_median_line_ambiguity, chinese_state_resource_access).
narrative_ontology:constraint_beneficiary(east_china_sea_median_line_ambiguity, japanese_state_resource_access).
narrative_ontology:constraint_victim(east_china_sea_median_line_ambiguity, maritime_law_credibility).
narrative_ontology:constraint_victim(east_china_sea_median_line_ambiguity, third_party_fisheries).
narrative_ontology:constraint_victim(east_china_sea_median_line_ambiguity, regional_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL FISHING COMMUNITIES (SNARE) — Small-scale fishers from Taiwan, Philippines, and Southeast Asia cannot exit territorial waters; face seizure, fines, and violence with no recourse. Maximum extraction with minimal coordination benefit. Trapped by geography and economic necessity.
constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: CHINA STATE (TANGLED ROPE) — Extracts hydrocarbon access and fishing zones through active enforcement (coast guard, military presence) while nominally adhering to UNCLOS framework. Coordination function exists (maritime boundary principle), but asymmetric extraction dominates. Exit constrained by domestic legitimacy narratives around 'historical rights' and nationalist resource security.
constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: JAPAN STATE (TANGLED ROPE) — Mirror position to China. Extracts median-line EEZ access and gas field rights through legal assertion and coast guard enforcement. Also benefits from UNCLOS coordination framework while extracting through asymmetric claims. Exit constrained by alliance obligations (US security guarantee) and domestic constituencies demanding energy security.
constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL LAW FRAMEWORK (SCAFFOLD) — UNCLOS, arbitration processes, and maritime law institutions provide scaffolding for boundary resolution. Low theater (law-based rather than performative) but operates under sunset logic: if China or Japan achieves political/military dominance, arbitration becomes irrelevant. Current function is genuine coordination toward stable boundaries, with exit available through institutionalization. Suppression is moderate — framework constrains both states from unilateral seizure but both can exit through non-compliance.
constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: UNCLOS MEDIAN LINE PRINCIPLE (PITON) — The abstract rule (EEZ boundaries derived from median line between baselines) is mathematically clean and theoretically sound. But the principle has degraded into performative citation: both China and Japan invoke it selectively while advancing contradictory baselines. The ritual persists (both cite UNCLOS frequently) but function is compromised. Theater high because legal argument obscures power asymmetries and resource extraction.
constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN VIEW) — From a pure geometric perspective, continental shelf boundaries in overlapping EEZ zones create inherent verification ambiguity: there is no natural fact of the matter about where the 'true' median line lies when baselines are contested. This appears as a natural law limit — the constraint emerges from mathematical structure, not institutional design. But the structural data reveals false summit: the ambiguity is instrumentalized by both states for extraction, making it a contingent geopolitical constraint, not a law of nature.
constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(east_china_sea_median_line_ambiguity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(east_china_sea_median_line_ambiguity, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(east_china_sea_median_line_ambiguity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(east_china_sea_median_line_ambiguity, TR),
    TR >= 0.70.

:- end_tests(east_china_sea_median_line_ambiguity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. The constraint extracts both resource access (hydrocarbon fields worth billions annually) and geopolitical advantage (leverage over regional states and alliance partners). The extraction is not maximal because both major states retain some coordination incentive (UNCLOS adherence maintains international legitimacy; arbitration remains possible). The measurement trajectory shows increasing extractiveness over the decade (0.35 → 0.58) reflecting China's growing military capacity to enforce its baseline claims unilaterally, reducing the negotiation requirement. Suppression (0.72): High. Small-scale fishers face violence, seizure, and economic coercion with minimal legal recourse. State-level suppression is also high—neither China nor Japan can credibly exit UNCLOS without major reputational costs, and both face domestic legitimacy constraints that prevent compromise (Chinese nationalism around 'historical rights'; Japanese alliance obligations and resource security narratives). The suppression mechanism is both structural (military enforcement, economic barriers) and institutional (UNCLOS commitment makes unilateral exit costly). Theater ratio (0.68): High and increasing. Legal argumentation about baselines and median line principles increasingly obscures the underlying power dynamics and resource extraction. Both states cite UNCLOS frequently, invoke arbitration rhetoric, and maintain the appearance of law-based dispute resolution while simultaneously conducting military enforcement and resource extraction. The performative content has grown as the legal complexity has increased while the actual coordination function (moving toward agreed boundaries) has stalled.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the snare perspective (regional fishers) and the tangled rope perspectives (China, Japan) is substantial. Fishers perceive pure extraction with no coordination benefit; states perceive genuine coordination necessity (maritime boundaries must be established) combined with extraction advantage. This gap is not a measurement error—it reflects real structural asymmetry: both states do benefit from some coordination mechanisms (UNCLOS prevents escalation to open conflict), while fishers receive zero coordination benefit (the boundaries, once established, will exclude them further, not enable their access). The gap between the tangled rope perspectives (states) and the scaffold perspective (law institutions) reveals that powerful actors see the constraint as providing extraction opportunity, while institutional mediators see it as a problem to be solved through arbitration. The gap between the piton perspective (UNCLOS principle persisting through theater) and the snare perspective (actual unilateral extraction happening on the ground) reveals that the performative legal framework obscures the material extraction dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position within the extraction flow. China and Japan are beneficiaries of baseline ambiguity (they can assert favorable boundaries and extract resources/geopolitical advantage), but their exit options are constrained—they cannot credibly exit UNCLOS without massive legitimacy costs. This produces moderate-to-high d values (~0.40-0.55 for each) reflecting that they experience extraction pressure (UNCLOS constraints) but benefit from the ambiguity's existence (continued baseline flexibility). Regional fishing communities are pure victims (zero beneficiary status) with trapped exit options, producing maximum d values (~0.95) reflecting that they bear extraction costs with no escape route. International law institutions are organized actors with mobile exit options; they are not beneficiaries of the ambiguity (they would prefer resolution) but they are not victims either (the law-based framework protects their legitimacy). This produces moderate d values (~0.50-0.60) reflecting their neutral structural position. The UNCLOS principle is an institutional abstraction with no embodied agent; it experiences no extraction in the material sense but its functional credibility is being degraded by state manipulation of the ambiguity, producing a synthetic d-equivalent of degradation (~0.60-0.70 reflecting that the principle is increasingly captured for extraction purposes). The analytical observer derives d from the full pattern of asymmetries, producing a d-equivalent of ~0.75 reflecting that the true structure of extraction is being obscured by the legal-principle framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is partially resolved here. This is not a case of false attribution to coordination (where extraction masquerades as cooperation). Rather, this is a case where genuine coordination function (UNCLOS boundary-setting) coexists with and is being instrumentalized for extraction. Both classification types are accurate from their respective perspectives: the constraint IS a tangled rope (coordination + extraction) from the state perspective, and it IS a snare (pure extraction) from the fishing community perspective. The analytical perspective risks a false mountain (treating the ambiguity as a natural law) when the ambiguity is actually maintained instrumentally. The constraint does not resolve to a single type—it is authentically a presheaf over the observation site. The mandatrophy reveals that the question 'is this coordination or extraction?' has different answers depending on who you are asking. For major states, it is coordinate-with-extraction. For powerless actors, it is extraction masquerading as coordination (international law provides them zero benefit). For international institutions, it is coordination (law-based resolution). The true structure is that the constraint provides coordination benefits among powerful actors while extracting from powerless ones—it is an extraction mechanism that coordinates among extractors. This is the canonical form of tangled rope, and it demonstrates that mandatrophy resolution is not about choosing one classification but about recognizing that different groups experience the constraint fundamentally differently.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    baseline_definition_ambiguity,
    'Which baseline should be used to calculate the median line: low-water mark, straight archipelago lines, or contested outer continental shelf claims?',
    'ICJ/ITLOS arbitration establishing binding baseline precedent; geological/hydrographic surveys establishing objective tidal reference',
    'Different baseline definitions shift the median line by 20-80 nautical miles. If baselines are fixed by arbitration: constraint shifts from snare/tangled_rope toward rope (coordination frame dominates). If baselines remain contested: extraction frame persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_definition_ambiguity, empirical, 'Baseline definition determines median line position by 20-80nm').

omega_variable(
    resource_distribution_beneath_ambiguity,
    'Are high-value hydrocarbon deposits (Shirakaba/Chunxiao fields) actually centered on the disputed boundary, or do resource surveys reveal unambiguous ownership?',
    'Seismic surveys establishing field geometries; geological continuity analysis determining resource ownership',
    'If resources straddle the boundary: extraction incentives are symmetric (both states have plausible claims). If resources are substantially on one side: extraction is asymmetric, favoring the state that controls that side.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resource_distribution_beneath_ambiguity, empirical, 'Whether hydrocarbon deposits straddle or fall cleanly within one state''s territory').

omega_variable(
    military_balance_stability_threshold,
    'At what military capability gap does China''s coast guard enforcement become irresistible, converting the constraint from tangled_rope (asymmetric but negotiable) to snare (unilateral extraction)?',
    'Comparative military capability assessment; empirical testing through incident escalation patterns; analysis of incident outcomes (seizures vs negotiated release)',
    'If gap is already decisive: constraint is effectively a snare (China extracting unilaterally). If gap remains contested: tangled_rope holds (both states still negotiating). Determines whether scaffold sunset is realistic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_balance_stability_threshold, empirical, 'Military balance threshold at which enforcement becomes unilateral').

omega_variable(
    unclos_exit_cost_asymmetry,
    'Would Japan''s exit from UNCLOS coordination (unilateral enforcement of its claimed baseline) impose higher legitimacy costs than China''s equivalent exit?',
    'Analysis of US alliance dependencies; assessment of international law community leverage; historical precedent from other UNCLOS disputes',
    'If exit costs are asymmetric: China faces lower reputational penalty for non-compliance, making unilateral extraction more rational. If symmetric: both states remain constrained by UNCLOS exit costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unclos_exit_cost_asymmetry, conceptual, 'Whether UNCLOS exit costs differ between China and Japan').

omega_variable(
    third_party_alliance_constraint,
    'Does US security guarantee to Japan function as external suppression of China''s extraction, or does US neutrality posture undermine the constraint?',
    'Analysis of US statements on ECS disputes; assessment of freedom of navigation operations impact; game-theoretic modeling of deterrence credibility',
    'If guarantee is credible: suppression of Chinese extraction keeps constraint in tangled_rope zone. If credibility degrades: China can extract unilaterally (snare), and the constraint''s type shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(third_party_alliance_constraint, empirical, 'Whether US alliance credibility functions as extraction suppression').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(east_china_sea_median_line_ambiguity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ecs_tr_t0, east_china_sea_median_line_ambiguity, theater_ratio, 0, 0.52).
narrative_ontology:measurement(ecs_tr_t5, east_china_sea_median_line_ambiguity, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ecs_tr_t10, east_china_sea_median_line_ambiguity, theater_ratio, 10, 0.68).
narrative_ontology:measurement(ecs_tr_t2, east_china_sea_median_line_ambiguity, theater_ratio, 2, 0.57).
narrative_ontology:measurement(ecs_tr_t7, east_china_sea_median_line_ambiguity, theater_ratio, 7, 0.65).

% Extraction over time
narrative_ontology:measurement(ecs_be_t0, east_china_sea_median_line_ambiguity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ecs_be_t5, east_china_sea_median_line_ambiguity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ecs_be_t10, east_china_sea_median_line_ambiguity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(ecs_be_t2, east_china_sea_median_line_ambiguity, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(ecs_be_t7, east_china_sea_median_line_ambiguity, base_extractiveness, 7, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(east_china_sea_median_line_ambiguity, enforcement_mechanism).
narrative_ontology:affects_constraint(east_china_sea_median_line_ambiguity, taiwan_strait_ambiguity).
narrative_ontology:affects_constraint(east_china_sea_median_line_ambiguity, south_china_sea_nine_dash_line).
narrative_ontology:affects_constraint(east_china_sea_median_line_ambiguity, island_dispute_entitlement_rules).

% DUAL FORMULATION NOTE:
% The ECS median line ambiguity is upstream of broader maritime boundary disputes in the region (Taiwan Strait, SCS nine-dash line claims). All three constraints share the instrumentalization of UNCLOS ambiguities for geopolitical extraction. The ECS constraint has the most symmetric power structure (two roughly equal states); the Taiwan Strait has asymmetric military power; the SCS involves multiple smaller states and a rising power. Decomposition is justified because the ε values differ: ECS ~0.58 (tangled rope dominant), Taiwan Strait ~0.71 (snare for smaller states), SCS ~0.65 (tangled rope with coalition dynamics).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(east_china_sea_median_line_ambiguity, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
