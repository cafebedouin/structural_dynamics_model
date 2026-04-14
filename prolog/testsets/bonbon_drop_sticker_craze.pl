% ============================================================================
% CONSTRAINT STORY: bonbon_drop_sticker_craze
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bonbon_drop_sticker_craze, []).

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
 *   constraint_id: bonbon_drop_sticker_craze
 *   human_readable: UHA Mikakuto's Bonbon Drop Collectible Sticker Promotion
 *   domain: economic/consumer_marketing
 *
 * SUMMARY:
 *   UHA Mikakuto's Bonbon Drop sticker promotion represents a structurally
 *   hybrid constraint that solves genuine coordination problems while
 *   simultaneously extracting significant value from child consumers and
 *   parental budgets. The constraint operates through deliberate collection
 *   incompleteness: rare stickers are seeded into candy boxes at varying
 *   probabilities, creating a psychological gap between 'have' and 'complete'
 *   that drives repeat purchasing. This mechanism satisfies coordination
 *   needs (children want engaging collectibles; companies want repeat
 *   purchasing incentives) but weaponizes psychological completion compulsion
 *   against consumers with low exit capacity. The constraint exhibits all six
 *   DR types depending on observer position: for the powerless child
 *   collector with trapped exit, it appears as pure extraction (Snare); for
 *   parents with constrained exit and some coordination benefit, it appears
 *   hybrid (Tangled Rope); for the company with arbitrage options, it appears
 *   as successful coordination (Rope); for the historical observer, it
 *   appears as degraded theater (Piton); for the regulatory movement with
 *   agency and sunset pathways, it appears temporary (Scaffold); for the
 *   psychological essentialist, it risks appearing as natural law (Mountain—a
 *   false summit).
 *
 * KEY AGENTS:
 *   - Child Collectors: Primary victims (powerless/trapped) — experience maximum extraction through completion compulsion; no exit capacity
 *   - Parents/Payers: Secondary victims (moderate/constrained) — bear financial and emotional costs; limited exit options (social pressure against refusal)
 *   - UHA Mikakuto: Primary beneficiary (institutional/arbitrage) — captures repeat purchasing revenue; has full mobility and can adjust promotion
 *   - Retailers: Secondary beneficiary (institutional/arbitrage) — benefit from increased foot traffic and repeat store visits
 *   - Peer Networks: Tertiary extractor (moderate/constrained) — social pressure to acquire rare stickers amplifies compulsion
 *   - Regulatory/Advocacy Bodies: Organized agent (organized/constrained) — see constraint as temporary with regulatory sunset pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bonbon_drop_sticker_craze, 0.52).
domain_priors:suppression_score(bonbon_drop_sticker_craze, 0.65).
domain_priors:theater_ratio(bonbon_drop_sticker_craze, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, extractiveness, 0.52).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(bonbon_drop_sticker_craze, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bonbon_drop_sticker_craze, tangled_rope).
narrative_ontology:human_readable(bonbon_drop_sticker_craze, "UHA Mikakuto's Bonbon Drop Collectible Sticker Promotion").
narrative_ontology:topic_domain(bonbon_drop_sticker_craze, "economic/consumer_marketing").

domain_priors:requires_active_enforcement(bonbon_drop_sticker_craze).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, uha_mikakuto).
narrative_ontology:constraint_beneficiary(bonbon_drop_sticker_craze, retailers).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, child_consumers).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, parental_budgets).
narrative_ontology:constraint_victim(bonbon_drop_sticker_craze, collection_incompleteness_victims).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE CHILD COLLECTOR (SNARE) — Child consumers experience maximum extraction through psychological completion compulsion. The sticker design and incomplete set create a trap: children cannot opt out of the collection logic once a few stickers are obtained. Suppression is high — peer pressure, fear of missing out, and the psychology of incomplete collections create powerful coercion. Exit is effectively unavailable (trapped); the child cannot afford to buy enough boxes, cannot earn money to continue purchasing, and cannot rationally stop mid-collection.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PARENT PAYER (TANGLED ROPE) — Parents experience mixed coordination and extraction. The promotion solves a real coordination problem: children want collectible items, and Bonbon Drop provides them. But parents also bear significant extraction costs — repeated candy purchases, spending discipline erosion, and managed disappointment when rare stickers don't appear. Parents have limited exit (constrained): they can refuse entirely, but face child disappointment and social isolation from peers. Some coordination benefit exists — the sticker system provides entertainment value — but extraction clearly exceeds coordination.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UHA MIKAKUTO & RETAILERS (ROPE) — From the company and retail perspective, the sticker system solves genuine coordination problems: enabling inventory movement, creating repeat purchasing, and building brand loyalty. The extraction runs toward these beneficiaries, but they experience the constraint as pure coordination — the mechanism works as designed and generates revenue. Arbitrage exit means they can shift to other promotions if this one fails; they have full mobility and benefit asymmetrically.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE HISTORICAL PATTERN OBSERVER (PITON) — From a civilizational view, collectible sticker promotions are a degraded, heavily theatrical version of earlier trading card and collectible systems (baseball cards, Pokemon cards). The theater_ratio is high because the sticker's functional utility (decoration/entertainment) is minimal; most value is psychological and social. The constraint persists through marketing inertia and proven effectiveness despite its theatrical character. Collectors and companies both maintain the system through institutional habit rather than novel function.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATION & AWARENESS MOVEMENT (SCAFFOLD) — Consumer protection organizations and parents' groups view the sticker promotion as a temporary marketing practice with declining social license. Regulations on predatory marketing to children, awareness campaigns about compulsive collection, and shift to digital alternatives are creating exit pathways. The constraint has a sunset: as parental awareness increases and regulatory frameworks tighten, the promotion's extraction mechanism will lose force. Sunset timeline: 5-15 years as regulatory frameworks mature and digital collectibles normalize.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: THE PSYCHOLOGICAL INVARIANT VIEW (MOUNTAIN) — From a deep psychological perspective, incomplete set completion is an immutable feature of human cognition and marketing psychology. The gap between 'have' and 'complete' triggers dopamine-driven purchasing behavior; the incomplete collection is inherently aversive to completion-oriented brains. This perspective risks naturalizing what is actually a contingent marketing choice (deliberately incomplete sets) as if it were a law of psychology. The engine's false summit detector will identify this as naturalization — incomplete collection design is engineered by the company, not inherent to stickers.
constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bonbon_drop_sticker_craze_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bonbon_drop_sticker_craze, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bonbon_drop_sticker_craze, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(bonbon_drop_sticker_craze, TR),
    TR >= 0.70.

:- end_tests(bonbon_drop_sticker_craze_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The promotion extracts significant value from child consumers and parents through psychology-based compulsion and incomplete set design. However, extraction is not maximal (≥0.70) because legitimate coordination benefits exist (children genuinely want collectibles; parents genuinely benefit from entertainment value and purchasing convenience). The company deliberately withholds certain stickers (rare variants) to sustain the completion gap; this is engineered extraction, not mere scarcity. Suppression (0.65): Moderate-high. Significant barriers to exit include peer pressure, FOMO (fear of missing out), psychological aversion to incomplete collections, and limited purchasing power for children to refuse. Parents face social pressure and child disappointment costs if they refuse. But suppression is not total (≤1.0) — parents retain choice, children can opt out entirely, and awareness campaigns are reducing social pressure. Theater ratio (0.58): Moderate-high. The sticker's functional utility (decoration) is minimal relative to its psychological and social signaling value. The constraint is increasingly theatrical as awareness grows that the 'rarity' is engineered rather than organic scarcity. The system persists through marketing institutional inertia rather than pure function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence because agents occupy fundamentally different exit and power positions. The child collector sees a trap (Snare) — no purchasing power, no exit, maximum compulsion. The parent sees mixed coordination and extraction (Tangled Rope) — they chose the purchasing mechanism but experience extraction pressure. The company sees successful coordination (Rope) — the mechanism solves their problem of repeat purchasing. The regulatory movement sees a temporary problem (Scaffold) — their organized power enables them to build exit pathways. The historical observer sees degraded theater (Piton) — the system works through momentum, not novel function. The psychological essentialist risks seeing natural law (Mountain) — but the structural data reveals this is naturalization of engineering choices. The gap between Snare (child victim, trapped) and Rope (institutional beneficiary, arbitrage) is maximal, making this an exemplary tangled_rope case.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position. Children occupy maximum-target position: low purchasing power, trapped by peer pressure and psychological completion need, high experienced extraction (d ≈ 0.95). Parents occupy mixed position: moderate power, constrained exit (can refuse but face costs), moderate experienced extraction (d ≈ 0.60). Companies occupy full-beneficiary position: institutional power, arbitrage exit (can shift promotions), low/negative experienced extraction (d ≈ 0.05). Peer networks operate as secondary extractors (d ≈ 0.70) — they apply pressure but are not the structural beneficiary. The engine's sigmoid function translates these d values into effective extractiveness chi: children experience high chi (peak experienced extraction); parents experience moderate chi; companies experience near-zero chi (extraction subsidizes them). Regulatory bodies with organized power and constrained exit occupy intermediate position (d ≈ 0.45) — they see the constraint from outside the compulsion loop but face institutional barriers to regulatory action.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by distinguishing between genuine coordination (children do benefit from collectible entertainment) and extractive layering (the incomplete set design and artificial rarity are added layers that transform coordination into extraction). The minimum viable coordination mechanism would be: 'candies include random stickers; children collect and trade.' The extractive layer is: 'certain stickers are deliberately made rare; completion is designed to be impossible without massive purchasing; rarity is engineered, not organic.' The tangled_rope classification captures this hybrid: the base mechanism is rope-level coordination (ε ≈ 0.15 if all stickers were equally common), but the deliberate rarity adds extraction (ε increases to 0.52). The snare classification from the child perspective is not false—it is the experienced constraint when exit is truly trapped. The rope classification from the company perspective is not false—extraction genuinely runs toward them. The gap reveals the same structural object (the sticker promotion) generates different effective constraints depending on agent power and exit options. The scaffold perspective introduces a temporal resolution: as digital alternatives and regulatory frameworks mature, the extraction mechanism will degrade (sunset), transforming this from tangled_rope back toward rope-only coordination or full regulatory exit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    completion_rate_threshold,
    'What completion rate (percentage of set obtained) triggers maximum compulsive behavior in child consumers?',
    'Behavioral economics studies on goal completion; analysis of purchasing patterns relative to sticker acquisition percentage',
    'If completion rate < 60%: many children never enter compulsive buying phase (reduces extraction). If rate > 80%: nearly all collectors experience severe compulsion (increases extraction and snare classification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(completion_rate_threshold, empirical, 'Threshold completion rate for maximum compulsive behavior').

omega_variable(
    peer_pressure_variability,
    'How much of the purchasing compulsion is genuine collection completion vs. peer pressure to possess specific rare stickers?',
    'Qualitative research with child collectors; comparison of purchasing behavior in isolated vs. peer-group contexts',
    'If primarily intrinsic completion: constraint is psychological universal (mountain-leaning). If primarily peer-driven: constraint is social/cultural (tangled_rope strengthened by network effects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(peer_pressure_variability, empirical, 'Attribution of compulsion to collection completion vs. peer pressure').

omega_variable(
    alternative_collectible_substitutability,
    'Are digital collectibles or other sticker systems sufficient substitutes to enable regulatory sunset, or is the physical sticker format structurally irreplaceable?',
    'Market analysis of digital collectible adoption in same demographic; regulatory trial programs with alternative collection systems',
    'If substitutable: scaffold sunset is credible (5-15 years). If irreplaceable: constraint will persist despite regulation (piton strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_collectible_substitutability, empirical, 'Whether digital collectibles can replace physical sticker collection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bonbon_drop_sticker_craze, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bonbon_tr_t0, bonbon_drop_sticker_craze, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bonbon_tr_t3, bonbon_drop_sticker_craze, theater_ratio, 3, 0.52).
narrative_ontology:measurement(bonbon_tr_t6, bonbon_drop_sticker_craze, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(bonbon_be_t0, bonbon_drop_sticker_craze, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bonbon_be_t3, bonbon_drop_sticker_craze, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(bonbon_be_t6, bonbon_drop_sticker_craze, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bonbon_drop_sticker_craze, resource_allocation).
narrative_ontology:affects_constraint(bonbon_drop_sticker_craze, pokemon_card_market_volatility).
narrative_ontology:affects_constraint(bonbon_drop_sticker_craze, gacha_game_mechanics).

% DUAL FORMULATION NOTE:
% The sticker promotion is a structural variant of broader collectible-compulsion mechanisms (trading cards, gacha games, loot boxes). The shared extraction principle is: incomplete sets + variable probability = compulsive purchasing. The sticker system has lower ε than gacha games (0.52 vs. 0.65+) because the sticker has lower functional value (mere decoration vs. gameplay utility) and lower price per unit. Network linkage indicates that understanding the sticker constraint informs analysis of higher-extractiveness collectible systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(bonbon_drop_sticker_craze, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
