% ============================================================================
% CONSTRAINT STORY: algorithmic_cultural_curation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_cultural_curation, []).

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
 *   constraint_id: algorithmic_cultural_curation
 *   human_readable: Algorithmic Cultural Curation and Visibility Extraction
 *   domain: digital_economics/cultural_distribution
 *
 * SUMMARY:
 *   Algorithmic cultural curation—the systematic ranking and distribution of
 *   creative content through opaque machine-learning systems—creates a
 *   structural extraction mechanism layered onto genuine coordination
 *   benefits. Platform algorithms solve a real problem: matching audience
 *   attention to content in information-abundant environments. But the
 *   solution concentrates visibility allocation power in unaccountable
 *   systems optimized for platform profit rather than cultural diversity or
 *   creator autonomy. The constraint exemplifies how Tangled Rope emerges
 *   from hybrid coordination-extraction: the algorithm genuinely reduces
 *   discovery friction for audiences and niche creators, but simultaneously
 *   enables asymmetric visibility control that extracts attention surplus
 *   from independent creators while suppressing cultural diversity that
 *   doesn't optimize for engagement metrics. The extractiveness has increased
 *   over the 15-year interval as algorithmic sophistication has deepened and
 *   platform dominance has consolidated. The theater ratio has risen as the
 *   algorithmic ranking process has become more opaque and
 *   performative—platforms maintain explanatory theater ('the algorithm is
 *   neutral,' 'recommendations are personalized') that obscures the genuine
 *   black-box optimization for platform metrics. The constraint demonstrates
 *   identity_locked exit dynamics: creators cannot imagine their cultural
 *   visibility existing outside algorithmic platforms because network effects
 *   have made algorithmic discovery the only path to contemporary cultural
 *   relevance. This is not a material barrier (they could theoretically use
 *   alternative platforms) but an internalized inevitability shaped by market
 *   concentration.
 *
 * KEY AGENTS:
 *   - Independent Creators: Primary victims (powerless/trapped) — structurally dependent on algorithmic visibility with no alternative reach; bear full extraction of attention surplus through invisibility penalty
 *   - Cultural Communities: Secondary victims (powerless/identity_locked) — culturally dependent on algorithmic platforms for contemporary visibility; identity as visible cultural actors depends on algorithmic ranking
 *   - Niche Creator Networks: Mixed position (moderate/constrained) — benefit from algorithmic discovery coordination but pay visibility tax; face high exit cost despite available alternatives
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture attention surplus and ranking monopoly; experience constraint as coordination mechanism; control all visibility allocation criteria
 *   - Algorithmic Gaming Actors: Powerful beneficiaries (powerful/mobile) — brands and well-capitalized creators can reverse-engineer ranking logic; experience mixed coordination and extraction asymmetry
 *   - Regulatory Coalition: Organized challengers (organized/constrained) — building transparency and interoperability mechanisms as sunset pathway; constrained by platform power but seeing structural exit pathway
 *   - Traditional Media Gatekeepers: Degraded competitors (institutional/arbitrage) — legacy editorial systems increasingly vestigial; maintain performative gatekeeping rituals with reduced functional necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent algorithmic optimization choices as inevitable properties of attention economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_cultural_curation, 0.58).
domain_priors:suppression_score(algorithmic_cultural_curation, 0.62).
domain_priors:theater_ratio(algorithmic_cultural_curation, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_cultural_curation, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_cultural_curation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(algorithmic_cultural_curation, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_cultural_curation, tangled_rope).
narrative_ontology:human_readable(algorithmic_cultural_curation, "Algorithmic Cultural Curation and Visibility Extraction").
narrative_ontology:topic_domain(algorithmic_cultural_curation, "digital_economics/cultural_distribution").

domain_priors:requires_active_enforcement(algorithmic_cultural_curation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_cultural_curation, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_cultural_curation, algorithmic_optimization_incentives).
narrative_ontology:constraint_victim(algorithmic_cultural_curation, independent_creators).
narrative_ontology:constraint_victim(algorithmic_cultural_curation, cultural_diversity).
narrative_ontology:constraint_victim(algorithmic_cultural_curation, algorithmic_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT CREATOR (SNARE) — Structurally trapped by platform dependence. Cannot reach audience except through algorithmic visibility allocation. No alternative distribution channel offers comparable reach. Faces algorithmic suppression without transparency, recourse, or ability to exit. Algorithm's opacity creates total informational asymmetry. Experiences maximum extraction: platform captures attention surplus generated by creator labor.
constraint_indexing:constraint_classification(algorithmic_cultural_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CULTURAL IDENTITY HOLDER (SNARE via IDENTITY_LOCKED) — Communities whose cultural expression depends on platform visibility. Structurally mobile (could use alternative media) but identity-locked through network effects: platform dominance means 'if you're not discoverable there, you don't exist in contemporary culture.' Identity as visible cultural agent requires participation. Exit would mean cultural erasure from mainstream discourse. Maximum extraction through internalized invisibility penalty.
constraint_indexing:constraint_classification(algorithmic_cultural_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: NICHE CREATOR COMMUNITY (TANGLED ROPE) — Benefits from algorithmic curation coordinating audience discovery within their niche. Reduced transaction costs of finding audience vs. pre-algorithm era. But benefits are asymmetrically distributed: platform captures ranking power, can de-prioritize any creator without cause. High exit cost (losing existing audience investment) but viable alternative communities exist (Patreon, Discord). Mixed coordination (solves discovery problem) and extraction (visibility allocation is black-box and uncontestable).
constraint_indexing:constraint_classification(algorithmic_cultural_curation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Solves genuine coordination problem: matching audience attention to content without algorithm would require manual editorial or market friction. Algorithm enables discovery at scale. Experiences constraint as coordination mechanism. Effective suppression of alternative visibility models (via network effects and algorithmic investment) appears as natural technical requirement rather than enforcement. Net beneficiary with low exit cost.
constraint_indexing:constraint_classification(algorithmic_cultural_curation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALGORITHMIC GAMING ACTOR (TANGLED ROPE) — Powerful agents (brands, influencers, well-capitalized studios) with resources to reverse-engineer algorithmic logic and optimize for visibility. Experience constraint as asymmetric coordination: they benefit from understanding algorithmic ranking (coordinate to maximize reach), while powerless creators cannot access same information. Mobile because they can afford to operate across platforms. Experience mixed extraction (some visibility taxed by algorithm) and coordination benefit (algorithm enables their reach).
constraint_indexing:constraint_classification(algorithmic_cultural_curation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (SCAFFOLD) — Organized actors (digital rights organizations, creator unions, media regulators) seeking to build transparency requirements and algorithmic auditing mechanisms. See the constraint as temporary: algorithmic accountability regulations (EU Digital Services Act, platform transparency mandates) are creating alternative visibility pathways with distributed curation logic. Exit mechanism: shift to platform neutrality obligations and creator-controlled recommendation systems. Sunset clause: transparency requirements and interoperability mandates will degrade platform's exclusive curation monopoly within 5-10 years.
constraint_indexing:constraint_classification(algorithmic_cultural_curation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TRADITIONAL MEDIA GATEKEEPER (PITON) — Legacy editorial and curation systems (broadcast television, print journalism, film studios) whose visibility allocation mechanisms are increasingly vestigial. Theater ratio is high: these organizations maintain gatekeeping rituals (editorial review, premiere sequences, publication workflows) that lack functional necessity in algorithmic ecosystem but persist through institutional inertia. They compete with algorithmic curation but largely on performance, not function. Extractive power is degraded but structure persists.
constraint_indexing:constraint_classification(algorithmic_cultural_curation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, attention scarcity appears as an immutable physical property: human attention is finite, so *some* distribution mechanism is necessary and *some* visibility allocation must be asymmetric. This perspective naturalizes algorithmic extraction as inherent to attention economics. The engine will identify this as a false summit, revealing that scarcity framing masks the contingent choice to concentrate curation power in unaccountable algorithms rather than distribute it through transparency, interoperability, or democratic governance.
constraint_indexing:constraint_classification(algorithmic_cultural_curation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_cultural_curation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_cultural_curation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_cultural_curation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_cultural_curation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_cultural_curation, TR),
    TR >= 0.70.

:- end_tests(algorithmic_cultural_curation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, increased over interval. The algorithm genuinely solves discovery coordination problems, which caps extractiveness below pure Snare (ε ≥ 0.66). But extractiveness is high because: (1) platform captures unilateral control over visibility allocation with zero transparency or recourse; (2) algorithmic optimization for engagement metrics systematically suppresses diverse content, reducing visibility for creators who don't align with engagement maximization; (3) measurement of extractiveness at interval-end (0.58) reflects maturity of algorithmic systems that have become more sophisticated and more monopolistic. Suppression (0.62): High. Multiple suppression mechanisms: structural dependence (creators trapped on platform due to network effects), informational (opacity of ranking logic prevents contestation), asymmetric (platform controls both the curation mechanism and the metrics it optimizes), and internalized (identity lock for culturally dependent communities). Suppression has remained relatively stable because it's structural to the platform model, not increasing with algorithmic sophistication. Theater ratio (0.65): Increasing from 0.40 to 0.72. The performative component is rising as platforms invest in explanatory theater ('recommendation transparency' dashboards, 'algorithmic literacy' campaigns) while maintaining black-box optimization. The theater masks the extractive optimization by framing algorithmic curation as neutral or inevitable. At interval-start, algorithmic systems were less sophisticated and claimed less neutrality. At interval-end, algorithmic theater has increased as platforms respond to critique by adding performative accountability rather than structural transparency.
 *
 * PERSPECTIVAL GAP:
 *   The independent creator sees Snare: black-box suppression with no exit. The platform operator sees Rope: solving coordination problem. The niche creator community sees Tangled Rope: mixed benefits and extraction. The regulatory coalition sees Scaffold: temporary problem with algorithmic transparency and interoperability as sunset. The algorithmic gaming actor sees modified Rope: they can coordinate within algorithmic logic to their advantage. The cultural identity holder sees Snare through identity_locked exit: culturally trapped by visibility dependence. The traditional media gatekeeper sees Piton: their own gatekeeping rituals are increasingly performative. The analytical observer risks seeing Mountain: attention scarcity as natural law justifying algorithmic distribution. The perspective gap reveals that a single structural phenomenon (algorithmic visibility allocation) produces contradictory classifications depending on the agent's power and exit options. The gap is not reconcilable—each perspective captures a real structural feature. The Snare is real for trapped creators; the Rope is real for platform operators; the Scaffold is real if regulatory coalitions succeed in building alternatives. The mandatrophy is resolved not by picking one type but by mapping each perspective's classification to the agent's power × exit options tuple.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators experience beneficiary d (derived from control of curation mechanism + arbitrage exit options) of approximately 0.10, producing f(d) ≈ -0.01 (institutional beneficiary). This is reflected in their Rope classification: they experience the constraint as low-extraction coordination. Independent creators experience target d (derived from trapped exit + victim status) of approximately 0.92, producing f(d) ≈ 1.38 (powerless target). This is reflected in their Snare classification: they experience maximum extraction. Niche creator communities experience moderate d (derived from constrained exit + mixed victim-beneficiary status) of approximately 0.55, producing f(d) ≈ 0.75 (moderate mixed). Regulatory coalitions experience organized d (constrained exit + oppositional beneficiary position) of approximately 0.35, producing f(d) ≈ 0.35 (organized constraint). The directionality derivation explains why the beneficiary sees coordination (Rope) while the victim sees extraction (Snare)—it's not that they disagree about the constraint's mechanics; it's that the constraint's extractiveness is directed at them from different sides. The beneficiary is upstream of the extraction flow; the victim is downstream.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CASE: Algorithmic curation initially resists classification as 'mere' extraction because it genuinely solves coordination problems. Platforms invest heavily in this claim: 'our algorithm matches audiences to content, a coordination service.' But the coordination benefit is asymmetric. The algorithm coordinates on *platform metrics* (engagement, watch time, clicks), not on creator welfare or cultural diversity. This is where mandatrophy resolution becomes critical: the constraint is Tangled Rope precisely because it contains both genuine coordination (discovery matching) AND asymmetric extraction (visibility monopoly + ranking opacity). The mandatrophy is resolved by acknowledging that Tangled Rope is the correct type—not 'is this coordination or extraction?' but 'this is hybrid coordination-extraction with unequal distribution of benefits.' The metrics prevent mislabeling as pure Rope (which would require symmetric beneficiary/victim profiles and low suppression) and prevent inflation to Snare (which would require zero genuine coordination function and χ ≥ 0.66). The tangled classification also flags the key mandatrophy danger: platforms can defend themselves against Snare accusations by pointing to genuine coordination benefits ('we help creators reach audiences'), creating cover story rhetoric that naturalizes extraction as necessary cost of coordination. The Tangled Rope classification admits the coordination is real while refusing the naturalizing frame.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_sufficiency,
    'Would meaningful transparency about algorithmic ranking actually enable creators to compete fairly, or does algorithmic complexity inherently prevent competitive reverse-engineering?',
    'Post-transparency longitudinal studies: comparison of visibility distribution before and after algorithmic audit requirements; measurement of how quickly gaming strategies emerge after transparency disclosure',
    'If transparency enables competition: constraint degrades toward Rope (coordination with informed participation). If transparency insufficient: constraint remains Snare/Tangled Rope (black-box power persists despite disclosure).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency enables fair creator competition').

omega_variable(
    algorithmic_diversity_trade_off,
    'Does algorithmic optimization for engagement inherently suppress diverse or challenging content, or does this represent contingent metric choice?',
    'Randomized algorithmic variants: test engagement-optimized vs diversity-optimized vs user-choice ranking on same content corpus; measurement of cultural representation distribution across ranking algorithms',
    'If inherent: suppression is technological necessity (Snare is justified). If contingent: suppression is choice (Snare is extractive and redesignable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_diversity_trade_off, empirical, 'Inherence of engagement-diversity trade-off in algorithmic ranking').

omega_variable(
    network_effects_exit_viability,
    'Can alternative platforms achieve sufficient audience density to enable creator exit from algorithmic curation monopoly, or do network effects mathematically prevent defection?',
    'Case analysis of alternative platforms (Mastodon, Bluesky, creator-owned networks) measuring growth rates, creator migration patterns, audience spillover; game-theoretic analysis of network effect breakpoints',
    'If viable exit: exit_options upgrade to mobile or arbitrage (constraint degrades). If mathematically locked: exit_options remain trapped or constrained (constraint severity persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effects_exit_viability, empirical, 'Viability of creator exit via alternative platforms').

omega_variable(
    identity_lock_cultural_persistence,
    'Does algorithmic invisibility constitute genuine cultural erasure for communities whose identity depends on contemporary visibility, or is visibility dependence itself a contingent technological condition?',
    'Historical comparison: how cultural identity formation processes differ across pre-algorithm, early-algorithm, and mature-algorithm periods; ethnographic analysis of whether algorithmic invisibility produces identity crisis or merely platform exit',
    'If genuine identity lock: suppression is internalized and persists even after platform exit. If contingent: suppression is external and reversible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_cultural_persistence, conceptual, 'Whether algorithmic invisibility produces identity lock or platform-contingent visibility dependence').

omega_variable(
    platform_optimization_constraints,
    'What are the actual technical constraints on implementing diversity-preserving or creator-controlled recommendation algorithms? Is current engagement-optimization the minimal necessary design or a contingent choice?',
    'Algorithmic audit of diversity-preserving ranking alternatives: measurement of feasibility, computational cost, and effectiveness of ranking systems that weight cultural diversity, creator sovereignty, or user-controlled curation',
    'If minimal necessary: current design is constrained by physics (mountain-like). If contingent choice: design is extractive optimization (Snare/Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_optimization_constraints, empirical, 'Technical necessity vs. contingency of engagement-optimized algorithmic design').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_cultural_curation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcc_tr_t0, algorithmic_cultural_curation, theater_ratio, 0, 0.4).
narrative_ontology:measurement(algcc_tr_t5, algorithmic_cultural_curation, theater_ratio, 5, 0.55).
narrative_ontology:measurement(algcc_tr_t10, algorithmic_cultural_curation, theater_ratio, 10, 0.65).
narrative_ontology:measurement(algcc_tr_t15, algorithmic_cultural_curation, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(algcc_be_t0, algorithmic_cultural_curation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algcc_be_t5, algorithmic_cultural_curation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(algcc_be_t10, algorithmic_cultural_curation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(algcc_be_t15, algorithmic_cultural_curation, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_cultural_curation, information_standard).
narrative_ontology:affects_constraint(algorithmic_cultural_curation, attention_economy_metric_collapse).
narrative_ontology:affects_constraint(algorithmic_cultural_curation, creator_economic_precarity).
narrative_ontology:affects_constraint(algorithmic_cultural_curation, cultural_diversity_suppression).

% DUAL FORMULATION NOTE:
% Algorithmic curation is upstream of three related constraints: (1) attention economy metric collapse—algorithmic optimization for engagement metrics produces distorted attention allocation; (2) creator economic precarity—algorithmic invisibility directly reduces creator income through visibility-dependent monetization; (3) cultural diversity suppression—algorithmic engagement optimization systematically de-prioritizes non-mainstream cultural expression. These are distinct constraints with different ε values but structurally coupled through the same algorithmic system. Algorithmic curation is the primary constraint; the others represent downstream extraction and diversity effects.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_cultural_curation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
