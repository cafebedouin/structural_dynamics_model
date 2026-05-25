% ============================================================================
% CONSTRAINT STORY: indo_russian_submarine_lease_2025
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_indo_russian_submarine_lease_2025, []).

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
 *   constraint_id: indo_russian_submarine_lease_2025
 *   human_readable: Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)
 *   domain: geopolitical/military_cooperation
 *
 * SUMMARY:
 *   The Indo-Russian nuclear submarine lease (Chakra III, Akula-class)
 *   represents a ~US$2 billion capability acquisition that entangles India in
 *   a hybrid coordination-extraction relationship with Russia, creates
 *   strategic costs for regional stability and US alliance cohesion, and
 *   masks Cold War dependency patterns in the language of temporary
 *   modernization. The constraint operates at the intersection of three
 *   dynamics: (1) India's need for rapid nuclear submarine capability to
 *   counter Chinese SSBN deployments and maintain regional deterrence; (2)
 *   Russia's need to monetize Cold War-era assets and maintain strategic
 *   influence in the Indo-Pacific as US-led alliances encircle it; (3) the US
 *   and its Quad partners' desire to integrate India into a collective
 *   security architecture while tolerating India's continued Russian defense
 *   dependencies. The constraint exhibits all six DR types from different
 *   structural positions, revealing the fundamental tension between India's
 *   simultaneous membership in competing strategic orders (Russia client, US
 *   ally, independent great power). Theater ratio (0.58) reflects the gap
 *   between stated intent (temporary lease for capability gap pending
 *   indigenous submarine maturation) and actual function (sustained strategic
 *   lock-in to Russian supplier and Cold War alignment patterns).
 *
 * KEY AGENTS:
 *   - Russia (Defense Establishment): Primary beneficiary (organized/arbitrage) — monetizes Cold War assets, maintains strategic influence, ensures India remains constrained customer
 *   - India (Naval Capability & Strategic Community): Mixed agent (powerful/constrained) — benefits from rapid capability acquisition but extracts cost via Russian dependency, technology transfer restrictions, strategic lock-in
 *   - Regional Strategic Stability: Primary victim (powerless/trapped) — Pakistan and other actors forced to respond with countervailing acquisitions; no exit options
 *   - US Alliance / Quad Cohesion: Secondary beneficiary + victim (institutional/constrained) — benefits from India's military modernization but extracts cost via India's strategic divergence, Russian supplier dependency, alliance management burden
 *   - China (Strategic Competitor): Implicit victim (powerful/constrained) — faces Indian naval capability acquisition but responds with countervailing deployments, triggering arms spiral
 *   - India's Indigenous Submarine Program: Alternative path (organized/mobile) — sunset clause mechanism; if maturation timeline compresses, makes Russian lease obsolete
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, 0.52).
domain_priors:suppression_score(indo_russian_submarine_lease_2025, 0.68).
domain_priors:theater_ratio(indo_russian_submarine_lease_2025, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, extractiveness, 0.52).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(indo_russian_submarine_lease_2025, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(indo_russian_submarine_lease_2025, tangled_rope).
narrative_ontology:human_readable(indo_russian_submarine_lease_2025, "Indo-Russian Nuclear Submarine Lease Agreement (Chakra III)").
narrative_ontology:topic_domain(indo_russian_submarine_lease_2025, "geopolitical/military_cooperation").

domain_priors:requires_active_enforcement(indo_russian_submarine_lease_2025).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, russian_defense_establishment).
narrative_ontology:constraint_beneficiary(indo_russian_submarine_lease_2025, indian_naval_capability_projection).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, regional_strategic_stability).
narrative_ontology:constraint_victim(indo_russian_submarine_lease_2025, us_alliance_cohesion).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: REGIONAL STRATEGIC STABILITY (SNARE) — The Indian subcontinent's strategic equilibrium bears the full cost of this asymmetric capability deployment. Pakistan and other regional actors cannot exit the constraint; they must respond with countervailing acquisition, escalating arms races. Zero exit options; maximum extraction in the form of forced capability acquisitions and regional destabilization. No beneficiary, only victim.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDIA / NAVAL CAPABILITY (TANGLED ROPE) — India benefits from acquisition of advanced SSN capability for regional deterrence and power projection. But the constraint is extractive: operational dependency on Russian logistics, technology transfer restrictions, maintenance of Cold War alignment patterns despite Quad membership tensions. Active enforcement required to maintain the arrangement against US Alliance pressure. Mixed: genuine coordination benefit (naval modernization) plus extraction (strategic lock-in to Russian supplier).
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: RUSSIA / DEFENSE ESTABLISHMENT (ROPE) — Russia coordinates with India to monetize Cold War-era submarine assets and maintain strategic influence in the Indo-Pacific. Russia has arbitrage options: can supply alternative customers, can adjust price, can leverage leverage for broader geopolitical concessions. Experiences the constraint as coordination mechanism — enabling export revenue, strategic positioning, and enforcement of Russian sphere-of-influence thinking.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: US ALLIANCE / QUAD COHESION (TANGLED ROPE) — The US and its alliance partners (Japan, Australia) benefit from India's military modernization and integration into Indo-Pacific security arrangements. But the submarine lease creates extraction: India's continued reliance on Russian technology and logistics contradicts US efforts to reduce India's strategic dependence on Moscow. Active enforcement of Quad norms (technology sharing, intelligence integration) requires managing the Russian constraint. Constrained exit: cannot eject India from Quad, cannot force abandonment of lease; must accept and work around.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: COLD WAR STRATEGIC LOGIC (PITON) — The submarine lease replicates Cold War technology transfer and client-patron patterns despite post-Cold War rhetoric. The constraint persists through institutional inertia: Russian supplier relationships, Indian naval planning cycles, established defense industrial partnerships. Theater is high (strategic independence narratives mask continued dependency). The Cold War logic is degraded (no longer functional for either Russia or India as originally designed), but the institutional form persists. Theater ratio reflects that much of the public framing is performative about 'independent naval development'.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INDIGENOUS SUBMARINE PROGRAMS (SCAFFOLD) — India's ongoing Arihant and Arihant-class indigenous nuclear submarine program offers an alternative exit path from Russian dependency. As India's domestic nuclear submarine capability matures (estimated maturation: 2030-2035), the need for Russian lease becomes temporary coordination support rather than permanent extraction. Sunset implicit: if indigenous capabilities prove reliable, the lease becomes obsolete. Organized agents (Indian Navy, strategic planners) recognize the temporary nature. Current classification as Scaffold with impending sunset as technology matures.
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the submarine lease is a structural adaptation to the US-China strategic competition in the Indo-Pacific. India cannot independently develop advanced nuclear submarines at the pace required by regional security dynamics. Russia provides both a coordination service (rapid capability acquisition) and extracts strategic lock-in cost (sustained dependency, limits on India's autonomous decision-making). The constraint is neither purely coordination (Rope) nor pure extraction (Snare) — it is asymmetric interdependence masquerading as temporary expedient. Theater ratio reflects the gap between stated intent (temporary lease for capability gap) and actual function (sustained strategic realignment).
constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_russian_submarine_lease_2025_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(indo_russian_submarine_lease_2025, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(indo_russian_submarine_lease_2025, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(indo_russian_submarine_lease_2025, TR),
    TR >= 0.70.

:- end_tests(indo_russian_submarine_lease_2025_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The lease involves genuine extraction of Indian strategic autonomy and policy space. Russia uses submarine dependency as leverage point for broader geopolitical concessions (India positions on Ukraine, China policy, Central Asian strategy). India captures real military capability but at cost of sustained supplier dependency despite rhetoric of 'temporary' arrangement. The value is higher than a simple technology transfer (0.25-0.35) because it involves active suppression of alternative paths (slow indigenous capability, delay Western integration) and enforcement of continued Russian strategic relationship. Suppression (0.68): High. Significant barriers include: (1) long timeline for indigenous capability maturation (15+ years); (2) Russian monopoly on advanced SSN technology willing to transfer to India; (3) Western export restrictions (MTCR, technology control regimes); (4) nuclear liability and operational complexity barriers to rapid indigenous development; (5) geopolitical pressure (US-Russia sanctions regime affects Russian willingness to supply); (6) career/institutional inertia in Russian-Indian defense partnerships. Theater ratio (0.58): Moderate-high. The constraint involves performative elements: public framing emphasizes India's 'temporary' capability gap and indigenous program maturity timeline, masking sustained strategic realignment toward Russia. But operational content is real (functioning submarine, genuine naval capability, real operational dependency). Theater has increased over the interval as the timeline for indigenous capability maturation has slipped repeatedly, exposing the 'temporary' framing as institutional rationalization.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence across structural positions. Russia sees pure coordination (Rope) — they are solving legitimate export revenue and strategic positioning problems with full arbitrage options. India sees mixed coordination and extraction (Tangled Rope) — genuine capability need but constrained by Russian dependency and US alliance tension. The Quad alliance sees extraction (Tangled Rope) — India's acquisition benefits regional security but extracts cost via strategic divergence and alliance management burden. Regional stability sees pure extraction (Snare) — forced to respond with countervailing acquisitions, no exit option. The Cold War logic sees its own degradation (Piton) — the institutional form persists through inertia despite post-Cold War rhetoric making the arrangement obsolete. The indigenous submarine program sees the constraint as temporary (Scaffold) — with implicit sunset as Indian capability matures. The analytical observer at civilizational scale sees tangled rope with masqueraded temporary intent — the gap between stated duration ('temporary lease') and actual function (sustained strategic lock-in) is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies sharply across agents. Russia as beneficiary with arbitrage options derives low d (0.10-0.20), experiencing negative effective extraction (institution benefits). India as victim-beneficiary with constrained exit derives moderate-high d (0.55-0.65), experiencing moderate extraction despite nominal benefit. Regional stability as pure victim with no exit derives d approaching 1.0, experiencing maximum extraction (snare classification). The Quad as institutional actor with constrained exit (cannot eject India, cannot force lease abandonment) derives d around 0.50-0.60, experiencing moderate extraction despite some benefit from India's capability acquisition. The Cold War logic as institutional residue derives d around 0.40 (organized power atom), experiencing moderate extraction as the arrangement persists through institutional inertia rather than genuine coordination function. India's indigenous submarine program as emerging alternative path derives low d (0.15-0.25, mobile exit), experiencing displacement from Russia's perspective as an alternative becomes viable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY STATUS: UNRESOLVED (extractiveness 0.52 < 0.70 threshold, but close). The constraint sits at the boundary between tangled rope and snare classification. Current mandatrophy analysis: The constraint resolves toward Tangled Rope (not pure Snare) because India genuinely benefits from rapid capability acquisition and Russia provides a coordination service that has no pure alternative (Western suppliers blocked by export controls, indigenous capability not mature enough). However, the extraction component is substantial enough to make the constraint more extractive than pure Rope — the suppression barriers (technology transfer restrictions, Western export controls, timeline pressures) are enforced to preserve Russian advantage. If extractiveness exceeds 0.70 in future measurement cycles (due to increased Russian coercion, failed Indian indigenous program, or US-India alliance breakdown), the constraint would require mandatrophy resolution as Snare. Current posture: Tangled Rope with high probability of transition to Snare if: (1) Russian coercion intensity increases; (2) US forces India to choose sides (abandons Quad); (3) China escalates regional arms race beyond Indian capability to respond.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indian_indigenous_capability_timeline,
    'When will India''s indigenous Arihant-class nuclear submarine program achieve reliable operational capability for sustained deployment?',
    'Tracking of Arihant sea trials, reactor performance data, launch intervals of subsequent boats, comparison of Indian vs international standards for SSN readiness',
    'If 2028-2030: Scaffold sunset becomes real, constraint transitions from tangled_rope to temporary coordination. If 2035+: Indian dependency on Russian lease extends beyond one generation, constraint remains tangled_rope or becomes piton (degraded but persistent institutional arrangement). If capability never reaches parity: constraint becomes permanent snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indian_indigenous_capability_timeline, empirical, 'Timeline for Indian indigenous nuclear submarine capability maturation').

omega_variable(
    us_quad_tolerance_threshold,
    'How much strategic divergence between India and the US on Russia relations can the Quad sustain before institutional breakdown?',
    'Analysis of Quad intelligence sharing protocols, technology transfer gates, joint military exercises, and stated divergences on Russia policy. Tracking of US statements on India-Russia defense ties and countervailing pressure mechanisms.',
    'If high tolerance (India can maintain Russian lease without Quad friction): constraint remains tangled_rope with contained extraction. If low tolerance (US compels India to choose): constraint transitions to snare (India trapped between Russian supplier and US alliance), or India exits US alignment entirely (becomes piton Cold War residue).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(us_quad_tolerance_threshold, empirical, 'Quad institutional tolerance for India-Russia strategic divergence').

omega_variable(
    russian_extraction_intensity,
    'Does Russia use submarine operations and logistics as leverage for broader geopolitical concessions beyond the lease, and at what intensity?',
    'Analysis of Russian statements linking submarine support to India positions on Ukraine, Central Asia, China policy. Tracking of maintenance delays, spare parts leverage, technology withholding relative to Indian diplomatic alignment.',
    'If extraction is primarily commercial (pure lease transaction): constraint is closer to Rope with some tangling. If Russia uses submarine dependency to coerce India policy positions: constraint is pure Snare from Russia''s perspective, with regional stability bearing the victim cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(russian_extraction_intensity, empirical, 'Extent of Russian strategic coercion via submarine lease dependency').

omega_variable(
    china_response_spiral_mechanism,
    'Does India''s submarine acquisition trigger Chinese strategic response (SSBN deployment, anti-submarine capability buildup) that escalates regional arms race?',
    'Tracking of Chinese naval deployments, anti-submarine exercise frequency, SSBN patrol patterns relative to Indian submarine acquisition timeline. Analysis of Chinese strategic documents on Indo-Pacific SSBN vulnerability.',
    'If China responds militarily: regional strategic stability victim status confirmed, constraint is snare from systemic perspective. If China responds diplomatically or accepts India capability: victim cost is lower, constraint is less extractive than current assessment (0.52).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(china_response_spiral_mechanism, empirical, 'China''s strategic response to Indian nuclear submarine acquisition').

omega_variable(
    technology_transfer_reversibility,
    'Can India reverse-engineer or domesticate Russian submarine technologies sufficiently to reduce supplier dependency over a 15-20 year horizon?',
    'Assessment of Indian indigenous submarine design sophistication, comparative analysis with Russian Akula-class architecture, tracking of technology transfer terms and Indian absorption capacity.',
    'If reversible (India achieves genuine technology sovereignty): constraint transitions from tangled_rope to temporary scaffold as indigenous capability enables exit. If irreversible (India locked into Russian dependency): constraint persists as tangled_rope or becomes piton (accepted permanent asymmetry).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_transfer_reversibility, empirical, 'Reversibility of Russian submarine technology dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(indo_russian_submarine_lease_2025, 2015, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(indorus_sub_tr_t0, indo_russian_submarine_lease_2025, theater_ratio, 0, 0.42).
narrative_ontology:measurement(indorus_sub_tr_t5, indo_russian_submarine_lease_2025, theater_ratio, 5, 0.54).
narrative_ontology:measurement(indorus_sub_tr_t10, indo_russian_submarine_lease_2025, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(indorus_sub_be_t0, indo_russian_submarine_lease_2025, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(indorus_sub_be_t5, indo_russian_submarine_lease_2025, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(indorus_sub_be_t10, indo_russian_submarine_lease_2025, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(indo_russian_submarine_lease_2025, enforcement_mechanism).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, quad_alliance_cohesion).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, china_ssbn_deployment_strategy).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, indo_pacific_arms_race_dynamics).
narrative_ontology:affects_constraint(indo_russian_submarine_lease_2025, indian_indigenous_submarine_program).

% DUAL FORMULATION NOTE:
% The submarine lease is downstream of broader India-Russia strategic relationship and US-China competition in Indo-Pacific. It is upstream of Indian indigenous submarine capability (alternative path) and regional arms race dynamics. The constraint family includes: (1) Indian indigenous submarine program (ε≈0.15, Scaffold, low extraction alternative path); (2) Quad alliance cohesion maintenance (ε≈0.48, Tangled Rope, institutional coordination with extraction); (3) China SSBN deployment strategy (ε≈0.42, Tangled Rope, response to Indian capability).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
