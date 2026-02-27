% ============================================================================
% CONSTRAINT STORY: narrative_capacity_exhaustion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_narrative_capacity_exhaustion, []).

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
 *   constraint_id: narrative_capacity_exhaustion
 *   human_readable: The Storytelling Burnout: Narrative Capacity Exhaustion
 *   domain: social/technological/creative_labor
 *
 * SUMMARY:
 *   The storytelling burnout constraint emerges at the intersection of
 *   algorithmic content ranking, attention scarcity, and creators' lived
 *   experience limits. What begins as a coordination mechanism—platforms
 *   matching audience appetite for narratives to creator supply—becomes a
 *   systematic extraction mechanism when algorithmic ranking rewards
 *   frequency over authenticity. The constraint manifests as a capacity
 *   mismatch: audiences demand continuous narrative fresh content; platforms
 *   amplify this demand through algorithmic visibility (high-frequency
 *   creators rank higher); creators face the structural pressure to produce
 *   narratives at a pace that outstrips their lived experience. The result is
 *   a treadmill where creators must fabricate, recycle, perform
 *   inauthenticity, or exit the system entirely. The theater ratio (0.64)
 *   reflects that much creator activity on algorithmic platforms is
 *   performative self-presentation (maintaining algorithmic visibility)
 *   rather than genuine storytelling. The extractiveness (0.52) captures the
 *   moderate but growing cost: platforms capture audience attention and
 *   advertiser revenue; creators capture diminished per-engagement
 *   compensation and exposure to algorithmic demotions if output frequency
 *   drops. The suppression (0.58) reflects significant barriers to exit:
 *   platform dependency for income, audience lock-in, and the mythologization
 *   of 'authentic' creation as unpaid labor (suppressing creator wage
 *   expectations). This constraint differs from a pure snare because
 *   platforms do provide genuine coordination benefits (audience access,
 *   content distribution infrastructure, monetization mechanisms); it differs
 *   from a pure rope because the benefits are systematically asymmetric and
 *   enforced through algorithmic ranking rather than mutual agreement.
 *
 * KEY AGENTS:
 *   - Content Creators (Individual): Primary victims (powerless/trapped) — bear extraction directly through unpaid labor, algorithmic visibility pressure, and burnout
 *   - Creator Communities/Unions: Secondary agents (moderate/constrained) — can partially organize and negotiate but face platform retaliation
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract attention and revenue; experience constraint as coordination problem
 *   - Algorithmic Ranking Systems: Institutional enforcer (powerful/arbitrage) — amplify extraction by frequency-weighting and engagement-maximization
 *   - Narrative Authenticity Commons: Powerless victim (powerless/trapped) — contaminated by fabrication pressure; no self-correction mechanism
 *   - Audience/Attention Brokers: Secondary beneficiaries (powerful/mobile) — benefit from content volume and engagement gamification
 *   - Alternative Narrative Platforms: Emerging organizers (organized/mobile) — building exits through decentralized and subscription models
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(narrative_capacity_exhaustion, 0.52).
domain_priors:suppression_score(narrative_capacity_exhaustion, 0.58).
domain_priors:theater_ratio(narrative_capacity_exhaustion, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, extractiveness, 0.52).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(narrative_capacity_exhaustion, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(narrative_capacity_exhaustion, tangled_rope).
narrative_ontology:human_readable(narrative_capacity_exhaustion, "The Storytelling Burnout: Narrative Capacity Exhaustion").
narrative_ontology:topic_domain(narrative_capacity_exhaustion, "social/technological/creative_labor").

domain_priors:requires_active_enforcement(narrative_capacity_exhaustion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, platform_operators).
narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, audience_attention_brokers).
narrative_ontology:constraint_beneficiary(narrative_capacity_exhaustion, algorithmic_ranking_systems).
narrative_ontology:constraint_victim(narrative_capacity_exhaustion, content_creators).
narrative_ontology:constraint_victim(narrative_capacity_exhaustion, narrative_authenticity).
narrative_ontology:constraint_victim(narrative_capacity_exhaustion, creator_wellbeing).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXHAUSTED CREATOR (SNARE) — Individual content creator facing algorithmic demand for constant narrative output. Trapped by: platform dependency for income, audience expectation lock-in, algorithmic visibility cliff if output frequency drops. No viable exit without loss of livelihood. Maximum experienced extraction — requires producing more stories than lived experience supports, forcing fabrication, recycling, or psychological collapse.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATOR COMMUNITY (TANGLED ROPE) — Organized but resource-constrained creators (unions, guild initiatives, creator advocacy groups) recognize both coordination benefits (platform access, audience reach) and extraction (unsustainable pacing, suppressed negotiating power). Can partially exit through collective action (negotiating contracts, demanding algorithm transparency) but faces platform retaliation and algorithm demotion. Mixed coordination/extraction—genuine benefits from platform infrastructure but systematic undercompensation for labor.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: creators' narratives are the content substrate that platforms convert to attention and engagement metrics. Platform benefits from the bottleneck (scarcity of human capacity justifies algorithmic curation) but frames it as solving a coordination problem (matching infinite audience demand to finite creator supply). Net beneficiary; extraction runs toward platform, not away.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALGORITHMIC RANKING SYSTEM (TANGLED ROPE) — The system that measures and rewards narrative output coordination function (routing scarce creator capacity to high-demand audience segments) but enforces extraction (frequency-weighted ranking incentivizes volume over depth; engagement-maximization penalizes authentic pacing). The ranking system sees itself as solving a coordination problem while actively amplifying extraction pressure. Active enforcement: algorithmic demotion of low-frequency creators, visibility cliffs on account inactivity.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ATTENTION ECONOMY RITUAL (PITON) — The broader institutional framing of creative labor as 'storytelling' (performative self-disclosure) rather than 'work' (compensated labor). Theater ratio high (0.64): much of creator activity is performative self-presentation to maintain algorithmic visibility; functional content (storytelling that actually meets audience needs) is buried under engagement theater. The ritual persists through institutional inertia—advertisers, platforms, and audiences maintain the fiction that 'authentic storytelling' is the value proposition while incentive structures enforce volume and speed.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NARRATIVE AUTHENTICITY AS TRAPPED POWERLESS AGENT (SNARE) — The epistemic commons of authentic storytelling (narratives grounded in lived experience) cannot exit the extraction mechanism and bears full cost. As pressure for volume increases, fabrication and recycling contaminate the narrative commons. No self-correction mechanism; false narratives persist and propagate. The collective good has no advocate, no exit, maximum extraction.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE NARRATIVE PLATFORMS (SCAFFOLD) — Decentralized storytelling networks, subscription-based creator platforms, and reader-supported media are building lower-pressure alternatives (Patreon, Substack, independent newsletters, diaspora protocols). These platforms have sunset clauses embedded: as creator income diversity increases and audience trust in algorithmic feeds erodes, the monopoly pressure of the primary platforms declines. Lower effective extraction because exits are materially available and multiplying.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, narrative capacity is a fixed human resource: the time available for authentic story creation is bounded by lifespan, attention, and lived experience. No amount of platform optimization can create stories from experience that hasn't happened. This perspective sees the bottleneck as an inherent constraint on human meaning-making, not a contingent institutional arrangement. However, structural data contradicts the mountain classification — the exhaustion is amplified by algorithmic ranking, platform monetization structure, and attention monopoly, not inherent to storytelling itself.
constraint_indexing:constraint_classification(narrative_capacity_exhaustion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(narrative_capacity_exhaustion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(narrative_capacity_exhaustion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(narrative_capacity_exhaustion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(narrative_capacity_exhaustion, TR),
    TR >= 0.70.

:- end_tests(narrative_capacity_exhaustion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Creators' labor is underpaid relative to audience value extracted (engagement, advertising revenue, data extraction); the monetization models (CPM, subscription share-backs, sponsorship) systematically undercompensate creator effort. The extractiveness is not as severe as pure labor exploitation (0.70+) because platforms do provide genuine distribution and monetization infrastructure that creators cannot easily replicate independently. The measurement (0.28→0.52 over the interval) reflects the intensification of algorithmic ranking pressure and the tightening compensation per creator as market saturation increases. Suppression (0.58): Moderate-high. Significant barriers to exit include: platform dependency for livelihood (sunk audience relationships), algorithmic visibility cliffs (inactivity penalties), audience expectation lock-in (followers expect regular uploads), and mythologization of creation as 'passion work' (suppressing wage expectations). Creators can exit but face meaningful costs. Theater ratio (0.64): High. Much creator activity on algorithmic platforms is performative self-presentation to maintain algorithmic visibility (posting cadence, algorithmic hook optimization, engagement bait) rather than authentic storytelling. The functional content (narratives that meet authentic audience needs) is buried under engagement theater. The ratio increased from 0.38 to 0.64 over the interval, reflecting the intensification of algorithmic ranking and the corresponding reduction in space for unoptimized narratives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a deep structural inversion between platform and creator perspectives. The platform's 'coordination function' (matching supply and demand) is experienced by creators as extraction (forced output at unsustainable pace). The creator's exit attempt (reducing output frequency to manage burnout) triggers algorithmic punishment (visibility cliff), which the platform frames as coordination maintenance ('keeping active creators visible'). The community organizer recognizes both: genuine platform value AND systematic pressure. The narrative authenticity commons is collateral damage—no agent advocates for it; it experiences pure extraction. The alternative platforms represent a real exit pathway with sunset logic: as creator income diversifies and audience trust in algorithmic feeds erodes, the treadmill's grip loosens. The piton classification reveals that much 'authentic storytelling' language is institutional ritual maintaining low creator wage expectations. The mountain perspective risks naturalizing narrative capacity as a fixed law when it is actually constrained by specific algorithmic design choices (frequency ranking, engagement maximization, per-creator demotion cliffs).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from structural position. Exhausted individual creators: beneficiary status → No (they receive minimal compensation), victim status → Yes (bear direct extraction), exit options → trapped (algorithmic visibility cliff if income-dependent). Result: d ≈ 0.90 (very high), f(d) ≈ 1.35 (very high experienced extraction). Platform operators: beneficiary status → Yes (capture attention and revenue), victim status → No, exit options → arbitrage (can adjust algorithms or shift revenue models freely). Result: d ≈ 0.08 (very low), f(d) ≈ -0.15 (negative—benefit, not extraction). Creator communities: beneficiary status → Partial (platform access, infrastructure), victim status → Partial (algorithmic pressure, low compensation), exit options → constrained (can organize and negotiate but face retaliation). Result: d ≈ 0.55 (moderate), f(d) ≈ 0.75 (moderate experienced extraction). Algorithmic systems: institutional power, arbitrage options, no direct harm experienced. Result: d ≈ 0.25 (low), f(d) ≈ 0.25. Narrative authenticity: powerless, no exit, maximum victim status. Result: d ≈ 0.95 (maximum), f(d) ≈ 1.40 (maximum). Alternative platforms: organized, mobile, exit available. Result: d ≈ 0.35 (low-moderate), f(d) ≈ 0.35 (low).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint is legitimately Tangled Rope, not a mislabeled Rope or Snare, because it meets all three gates: (1) Coordination function: platforms do solve a real matching problem (audience demand for narratives ↔ creator supply). (2) Asymmetric extraction: beneficiaries (platform, algorithm, audience) systematically extract from creators through underpayment and burnout. (3) Active enforcement: algorithmic ranking actively amplifies extraction through frequency-weighting and inactivity penalties. The mandatrophy analysis asks: Could this be a pure Rope (coordination with minimal extraction)? No—suppression (0.58), beneficiary asymmetry, and algorithmic enforcement all preclude pure coordination. Could this be a pure Snare (extraction without coordination)? No—platforms genuinely do provide distribution, monetization, and audience access that creators cannot easily replicate. The Tangled Rope classification is stable across perspectives that experience the constraint's mixed nature (creator communities, algorithmic systems). Perspectives that experience it as pure extraction (exhausted creators, narrative authenticity) or pure coordination (platforms) are seeing the constraint through a lens that captures their structural position but not its full topology. The constraint IS both coordination and extraction—the two functions are coupled, not separated. The extraction is enabled by the coordination infrastructure; the coordination function justifies the extraction pressure. Disentangling them would require either (a) reducing algorithmic frequency-ranking (accepting lower engagement metrics), (b) increasing creator compensation (reducing platform margin), or (c) building alternative platforms with different incentive structures (the scaffold exit path).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authenticity_measurability,
    'Can narrative authenticity (grounding in lived experience) be reliably measured or detected by algorithmic systems?',
    'Linguistic analysis of authentic vs fabricated narratives; human rater correlation with fabrication signals; LLM detection accuracy on creator testimony',
    'If measurable: platforms could introduce authenticity weighting to reduce fabrication incentives. If unmeasurable: the constraint is structural—no feedback loop can prevent the race to volume from contaminating authenticity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authenticity_measurability, empirical, 'Whether algorithmic systems can detect authentic narratives').

omega_variable(
    saturation_threshold,
    'Is there a critical saturation point where audience demand exceeds available narrative supply, forcing platform algorithm changes?',
    'Longitudinal tracking of creator burnout rates, platform engagement per-creator ratios, audience satisfaction with content freshness; threshold identification where engagement stops increasing with frequency',
    'If threshold exists and is reached: platforms will shift from frequency-ranking to quality/authenticity-ranking to manage supply. If no threshold: extraction pressure continues to rise indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(saturation_threshold, empirical, 'Whether audience saturation forces algorithmic changes').

omega_variable(
    alternative_platform_viability,
    'Can decentralized or subscription-based storytelling platforms achieve critical mass sufficient to absorb creator exit from algorithmic platforms?',
    'Growth tracking of Substack, Patreon, diaspora networks, and independent creator income stability; correlation with algorithmic platform creator satisfaction decline; revenue per creator comparison',
    'If viable: scaffold sunset is real—alternative platforms will absorb creator migration. If not viable: creators remain trapped; apparent exits are illusory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms can absorb creator exit').

omega_variable(
    labor_vs_performance_framing,
    'Is the core extraction mechanism the underpayment of labor (insufficient creator compensation relative to audience value extracted) or the conflation of work with performance (narrative production framed as authentic self-disclosure rather than compensated labor)?',
    'Comparison of creator wellbeing in platforms with direct labor contracting (hourly/salaried creators) vs engagement-based monetization; analysis of creator narratives about ''passion work'' vs ''job exhaustion''',
    'If labor underpayment: regulation should mandate minimum compensation guarantees. If performance conflation: regulation should require transparent labor classification. Different impacts suggest different constraint types.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_vs_performance_framing, conceptual, 'Whether extraction is wage-suppression or performance misclassification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(narrative_capacity_exhaustion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(narr_tr_t0, narrative_capacity_exhaustion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(narr_tr_t5, narrative_capacity_exhaustion, theater_ratio, 5, 0.51).
narrative_ontology:measurement(narr_tr_t10, narrative_capacity_exhaustion, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(narr_be_t0, narrative_capacity_exhaustion, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(narr_be_t5, narrative_capacity_exhaustion, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(narr_be_t10, narrative_capacity_exhaustion, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(narrative_capacity_exhaustion, information_standard).
narrative_ontology:affects_constraint(narrative_capacity_exhaustion, algorithmic_ranking_capture).
narrative_ontology:affects_constraint(narrative_capacity_exhaustion, creator_labor_classification).
narrative_ontology:affects_constraint(narrative_capacity_exhaustion, attention_monopoly).

% DUAL FORMULATION NOTE:
% The storytelling burnout has two structurally related but distinct upstream constraints: (1) Algorithmic ranking capture (the constraint that platforms use frequency-weighted ranking to maximize engagement, which amplifies output pressure), and (2) Creator labor misclassification (the constraint that creation is framed as 'passion work' rather than compensated labor, suppressing wage expectations). This story captures the downstream effect of both—the systemic pressure on narrative capacity. Separate stories for the upstream constraints would show lower extractiveness values but explain how they couple to produce the tangled rope observed here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(narrative_capacity_exhaustion, powerful, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
