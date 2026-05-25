% ============================================================================
% CONSTRAINT STORY: polarization_acceleration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_polarization_acceleration, []).

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
 *   constraint_id: polarization_acceleration
 *   human_readable: Polarization Acceleration in Democratic Discourse
 *   domain: political/social/technological
 *
 * SUMMARY:
 *   Polarization acceleration is a structural constraint where algorithmic
 *   ranking, media economics, and political incentive structures combine to
 *   suppress moderate discourse and amplify extreme viewpoints. This
 *   constraint operates across multiple institutional levels simultaneously:
 *   platform algorithms (technical), media production (economic), political
 *   mobilization (organizational), and individual preference formation
 *   (cognitive). The constraint exhibits the full spectrum of Deferential
 *   Realism classifications depending on the observer's structural position.
 *   From the perspective of moderate citizens, it is a snare: they are
 *   trapped in systems that suppress their preferred content and amplify
 *   opposition. From extremist actors' perspective, it is pure coordination:
 *   they leverage polarized messaging to mobilize supporters. From platform
 *   and extremist media perspectives, it is rope: the constraint solves
 *   genuine coordination problems (efficient signal routing, base
 *   mobilization). From centrist political actors, it is tangled rope: they
 *   must use the same polarizing channels to reach constituents, experiencing
 *   both coordination benefit and extraction cost. From deliberative reform
 *   movements, it is a temporary scaffold with an exit path: alternative
 *   communication architectures can bypass polarization acceleration. From
 *   traditional news media, it is a degraded piton: journalism norms persist
 *   performatively while their function has been usurped by algorithmic
 *   ranking. The analytical observer sees the full tangled rope: genuine
 *   coordination function (rapid coalition formation, distributed political
 *   participation) coupled with asymmetric extraction (concentration of power
 *   in polarization-expertise actors). The constraint's theater_ratio
 *   (declining from 0.62 to 0.48) reflects that the overt performative
 *   elements (debate rituals, fact-checking segments, balanced reporting
 *   norms) have been stripped away — what remains is the raw
 *   engagement-maximization and polarization-acceleration mechanism with
 *   minimal theatrical covering. This decline in theater simultaneously marks
 *   the constraint becoming more functionally extractive (base extractiveness
 *   rising from 0.32 to 0.58).
 *
 * KEY AGENTS:
 *   - Moderate Citizens: Primary victim (powerless/trapped) — systematically suppressed in algorithmic ranking; no exit from digital infrastructure; bears maximum extraction cost through cognitive capture and preference distortion
 *   - Extremist Political Actors: Primary beneficiary (powerful/arbitrage) — coordinate mobilization through polarized messaging; high exit capacity; net beneficiaries of accelerated polarization
 *   - Algorithmic Platforms: Primary beneficiary institution (institutional/arbitrage) — benefit from polarization-driven engagement; high arbitrage capacity; active enforcement of polarization-maximizing algorithms
 *   - Centrist Political Actors: Secondary victim (moderate/constrained) — must operate through polarized channels; moderately suppressed by algorithmic ranking; exit possible but costly
 *   - Extremist Media Outlets: Secondary beneficiary (institutional/arbitrage) — flourish in polarized information environment; active enforcement through sensationalism and moral outrage narratives
 *   - Deliberative Reform Movements: Organized reformers (organized/constrained) — building alternative communication architectures; perceive sunset clause; moderate suppression; see agency and exit path
 *   - Traditional News Media: Institutional actor (institutional/constrained) — professional norms (journalism, fact-checking) persist but have lost functional force; maintained through inertia; partial exit to alternative models possible
 *   - Analytical Observer: Civilizational vantage (analytical/analytical) — sees full tangled rope structure with genuine coordination function underlying asymmetric extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(polarization_acceleration, 0.58).
domain_priors:suppression_score(polarization_acceleration, 0.62).
domain_priors:theater_ratio(polarization_acceleration, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(polarization_acceleration, extractiveness, 0.58).
narrative_ontology:constraint_metric(polarization_acceleration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(polarization_acceleration, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(polarization_acceleration, tangled_rope).
narrative_ontology:human_readable(polarization_acceleration, "Polarization Acceleration in Democratic Discourse").
narrative_ontology:topic_domain(polarization_acceleration, "political/social/technological").

domain_priors:requires_active_enforcement(polarization_acceleration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(polarization_acceleration, algorithmic_curators).
narrative_ontology:constraint_beneficiary(polarization_acceleration, extremist_media_outlets).
narrative_ontology:constraint_beneficiary(polarization_acceleration, political_actors_with_high_salience).
narrative_ontology:constraint_victim(polarization_acceleration, centrist_consensus_builders).
narrative_ontology:constraint_victim(polarization_acceleration, deliberative_discourse_norms).
narrative_ontology:constraint_victim(polarization_acceleration, cross_factional_trust).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MODERATE CITIZEN (SNARE) — Citizens seeking centrist information or consensus-building face systematic suppression of their preferred discourse. Algorithmic feeds, platform ranking, and media economics all extract moderate content from visibility while promoting extreme viewpoints. No exit: one cannot escape digital social infrastructure; passive consumption offers no alternative. Bears maximum extraction.
constraint_indexing:constraint_classification(polarization_acceleration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EXTREMIST POLITICAL ACTORS (ROPE) — Coordinate mobilization of supporters through polarized messaging. Experience the constraint as pure coordination: rallying base, funding, and action. High exit capacity (can shift to alternative platforms, media, or organizing structures). Net beneficiaries.
constraint_indexing:constraint_classification(polarization_acceleration, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ALGORITHMIC PLATFORMS (TANGLED ROPE) — Possess genuine coordination function (connecting users, enabling expression at scale, routing information). BUT: engagement-maximization algorithms simultaneously extract value from polarization. The platform benefits from coordination services (subscriber growth, network effects, advertising inventory) while actively amplifying polarizing content because it generates higher engagement. Requires active enforcement of ranking algorithms that prioritize engagement over social resilience. Asymmetric extraction disguised as neutral coordination.
constraint_indexing:constraint_classification(polarization_acceleration, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CENTRIST POLITICAL ACTORS (TANGLED ROPE) — Must coordinate through the same polarized channels as extremists. Genuine coordination benefit (reach constituents, mobilize voters). BUT: constrained by algorithmic economics — centrist messaging generates lower engagement, receives lower algorithmic promotion. Active enforcement of ranking algorithms suppresses their signal. Moderate extraction: can exit (alternative media, direct outreach) but at high cost. Not trapped, but significantly constrained.
constraint_indexing:constraint_classification(polarization_acceleration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TRADITIONAL NEWS MEDIA (PITON) — Journalism norms (balanced reporting, differentiation of news/opinion, fact-checking) once served as friction against polarization. These norms persist as institutional practice but have lost functional force: they are drowned out by algorithmic amplification of sensationalism and polarization. Theater ratio high: editorial deliberation, fact-checking bureaus, ethics codes continue while their effectiveness has collapsed. Maintained through institutional inertia and professional identity; functional verification of the constraint has atrophied.
constraint_indexing:constraint_classification(polarization_acceleration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DELIBERATIVE REFORM MOVEMENTS (SCAFFOLD) — Organized actors (Bridge Alliance, Common Ground, deliberative democracy initiatives) are building alternative communication architectures: citizen assemblies, structured dialogues, moderated forums with norms against polarization. See the acceleration constraint as temporary and surmountable through institutional redesign. Low effective extraction because they perceive agency and an exit path. Has sunset clause: as these alternatives gain adoption, polarization acceleration loses leverage.
constraint_indexing:constraint_classification(polarization_acceleration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational vantage: polarization acceleration has both a genuine coordination function (rapid coalition formation, mobilization of distributed support networks, efficient routing of political attention) and asymmetric extraction (captures and concentrates political power in the hands of actors who master polarizing rhetoric). The constraint is neither pure coordination nor pure extraction — it genuinely does solve collective action problems while simultaneously extracting from moderate actors and deliberative norms. The chi formula applies with full force: base extractiveness 0.58, multiplied by f(d) and scope factors across different agent positions.
constraint_indexing:constraint_classification(polarization_acceleration, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(polarization_acceleration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(polarization_acceleration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(polarization_acceleration, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(polarization_acceleration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(polarization_acceleration, TR),
    TR >= 0.70.

:- end_tests(polarization_acceleration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, and rising over the interval. Initial extractiveness (0.32) reflects the constraint's early stage when traditional media gatekeeping was still effective at moderating polarization. As algorithmic ranking displaced editorial judgment (interval midpoint 0.45), extractiveness increased. Current level (0.58) reflects the constraint's mature form where engagement maximization directly drives polarization without institutional friction. The extraction is not absolute (platforms still provide coordination services, extremists still must invest in persuasion, infrastructure has costs) but sustained and structural. Suppression (0.62): Moderate-high and stable. Barriers to moderate discourse include algorithmic demotion, engagement-disadvantage (polarized content generates 5-8x higher engagement), media business model dependence on attention, and increasingly, cognitive capture (users internalize preference for polarized content). These are substantial but not total — moderate voices persist and can organize alternatives. Theater ratio (declining from 0.62 to 0.48): Reflects the constraint's maturation. Early-stage polarization acceleration was partially masked by journalistic performance (balanced reporting, editorial standards, fact-checking). These performative elements have been stripped away as algorithmic ranking has made them irrelevant — what remains is the raw mechanism without dressing. This decline in theater is paradoxically a sign of the constraint becoming more functionally transparent and more effective at extraction.
 *
 * PERSPECTIVAL GAP:
 *   The gap between moderate citizen (snare) and extremist actor (rope) perspectives is the key diagnostic. Both use the same constraint (algorithmic ranking, engagement maximization) but experience opposite classifications. This gap is not due to measurement error or ambiguous definition — it reflects genuine structural asymmetry in who controls the constraint's mechanisms and who bears its costs. The moderate citizen sees the constraint as immutable (snare) because they are structurally powerless to change platform ranking algorithms or media economics. The extremist actor sees it as pure coordination (rope) because they benefit from and can leverage the constraint's operation. The platform sees it as tangled rope: they genuinely do solve coordination problems (connecting users at scale, routing information) while simultaneously extracting value from polarization. The analytical observer's tangled rope classification is not a compromise or averaging of these perspectives — it is the structural reality that resolves the gap: the constraint has BOTH a genuine coordination function AND an asymmetric extraction mechanism, and these are not in conflict. The existence of the coordination function is precisely what makes the extraction sustainable. Without the real benefit (reaching supporters, rapid mobilization), the constraint would be detectable as pure snare and would collapse. With only the benefit visible, it would be classified as rope. Tangled rope correctly captures the hybrid structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from their structural position in the polarization flow. Moderate citizens are victims with no exit (d ≈ 0.95, f(d) ≈ 1.42) — trapped in platforms, suppressed by ranking, unable to organize alternatives at scale. Extremist actors are beneficiaries with high arbitrage (d ≈ 0.10, f(d) ≈ -0.02) — extract value from polarization while maintaining ability to shift to alternative mobilization channels. Platforms are beneficiaries with arbitrage (d ≈ 0.15, f(d) ≈ -0.01) — benefit from engagement amplification, can theoretically optimize for different objectives, but choose polarization because it maximizes measurable metrics. Centrist actors are victims with constrained exit (d ≈ 0.65, f(d) ≈ 1.02) — bear costs of suppression but retain some ability to mobilize through parallel channels and alternative media. The perspectival gap reveals the constraint's asymmetry: those who experience maximum extraction (moderate citizens) have no exit; those with highest exit capacity (extremists, platforms) are maximum beneficiaries. This inversion of power and exit capacity is the structural signature of tangled rope becoming snare from the weakest position.
 *
 * MANDATROPHY ANALYSIS:
 *   Polarization acceleration resolves the mandatrophy through perspectival gap analysis. The constraint cannot be classified as a single type because its fundamental structure is asymmetrically beneficiary-victim related. From the beneficiary's view (extremist actor, platform, extremist media), it genuinely solves coordination problems — it is rope-like. From the victim's view (moderate citizen, centrist actor, deliberative norms), it is pure extraction — it is snare-like. The analytical observer's tangled rope classification does not average these perspectives; it identifies the structural reality that explains why the gap exists: the constraint has a real coordination function that benefits some actors while extracting from others. The mandatrophy is resolved when the engine recognizes that (1) the coordination function is genuine — the constraint does enable rapid coalition formation and message amplification, and (2) the extraction is structural — the asymmetry is built into the mechanism, not accidental. The fact that extremists benefit from the same mechanism that harms moderates is not a bug or a measurement error; it is the structure of the constraint itself. The piton perspective (degraded journalism norms) and scaffold perspective (deliberative alternatives) provide temporal resolution: over time, the constraint's theater ratio declines (performative journalism becomes irrelevant) while extractiveness rises (raw mechanism becomes dominant), and simultaneously, alternative pathways (deliberative platforms) develop that can eventually sunset the acceleration constraint. This temporal story resolves the mandatrophy: the constraint was never purely coordination (rope), nor is it likely to become purely extractive (snare) — it is tangled rope in its mature form, with both functions operating simultaneously, and alternatives developing that could eventually displace it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_intentionality,
    'Is polarization acceleration a designed feature of ranking algorithms or an emergent consequence of engagement maximization?',
    'Platform documentation analysis, algorithm audit data, internal research papers (leaked or released), A/B testing records showing engagement vs. polarization tradeoffs',
    'If designed: classification as intentional snare may be warranted, extraction χ rises. If emergent: constraint is tangled rope (coordination function with unintended extraction). If mixed: requires decomposition into separate designed vs. emergent stories.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_intentionality, empirical, 'Whether polarization is designed feature or emergent consequence of engagement optimization').

omega_variable(
    user_preference_causal_direction,
    'Do algorithmic rankings reflect genuine user preferences for polarized content, or do they manufacture those preferences through repeated exposure?',
    'A/B testing with neutral-ranking controls, historical data on preference shifts after algorithm changes, cognitive capture analysis (user stated vs revealed preferences), alternative platform experiments with depolarizing ranking',
    'If algorithms reflect preferences: constraint is coordination mechanism (users getting what they want); extraction is lower. If algorithms manufacture preferences: constraint is snare (users trapped in preference loops); extraction is higher and suppression more severe.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_preference_causal_direction, empirical, 'Causal direction of user preference and algorithmic ranking').

omega_variable(
    centrist_exit_feasibility,
    'Can centrist actors and moderate users credibly build parallel information infrastructure at scale, or is network effect lock-in insurmountable?',
    'Case studies of alternative platforms (Bluesky, Mastodon, community forums); measurement of adoption rates and retention; cost analysis of achieving population scale; network effect modeling',
    'If feasible: scaffold perspective confirmed, exit_options upgrade from constrained to mobile. If infeasible: centrist actors remain trapped in polarized platforms; constraint remains snare from their perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(centrist_exit_feasibility, empirical, 'Whether centrist actors can credibly exit to alternative platforms').

omega_variable(
    cross_factional_trust_recovery_timeline,
    'How long does it take for cross-factional trust to rebuild after polarization acceleration? Is recovery possible without institutional intervention?',
    'Longitudinal trust surveys, historical analysis of polarization-depolarization cycles, measurement of spontaneous trust recovery vs. deliberative intervention outcomes',
    'If recovery is fast: temporary suppression effect (constraint is scaffold). If recovery requires generational timescale or external intervention: suppression is structural (constraint is snare or piton).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_factional_trust_recovery_timeline, empirical, 'Timeline and feasibility of cross-factional trust recovery').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(polarization_acceleration, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(polar_tr_t0, polarization_acceleration, theater_ratio, 0, 0.62).
narrative_ontology:measurement(polar_tr_t3, polarization_acceleration, theater_ratio, 3, 0.55).
narrative_ontology:measurement(polar_tr_t6, polarization_acceleration, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(polar_be_t0, polarization_acceleration, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(polar_be_t3, polarization_acceleration, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(polar_be_t6, polarization_acceleration, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(polarization_acceleration, attachment_coordination).
narrative_ontology:affects_constraint(polarization_acceleration, algorithmic_engagement_optimization).
narrative_ontology:affects_constraint(polarization_acceleration, media_business_model_fragmentation).
narrative_ontology:affects_constraint(polarization_acceleration, epistemic_closure_information_silos).

% DUAL FORMULATION NOTE:
% Polarization acceleration is downstream of algorithmic engagement optimization and upstream of epistemic closure. The engagement optimization constraint (technical) has its own ε reflecting platform design choices; polarization acceleration (behavioral) reflects how those choices interact with political incentives and media economics; epistemic closure (cognitive) reflects how accelerated polarization hardens identity and worldview boundaries. Each story has distinct ε values and should be consulted separately. Network links show causal influence: engagement optimization enables polarization acceleration, which drives epistemic closure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(polarization_acceleration, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
