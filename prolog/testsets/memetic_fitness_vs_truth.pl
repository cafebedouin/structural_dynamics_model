% ============================================================================
% CONSTRAINT STORY: memetic_fitness_vs_truth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_memetic_fitness_vs_truth, []).

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
 *   constraint_id: memetic_fitness_vs_truth
 *   human_readable: The Viral Distortion: Memetic Fitness Decoupling from Truth
 *   domain: social/informational/technological
 *
 * SUMMARY:
 *   The memetic fitness versus truth constraint describes the structural
 *   decoupling between the properties that cause an idea to spread virally
 *   within digital-social substrates and the properties that correspond to
 *   accurate representation of reality. This constraint exhibits a
 *   perspectival spectrum from pure extraction (snare for powerless consumers
 *   and the epistemic commons) through mixed coordination-extraction
 *   (tangled_rope for reality-grounded communities) to pure coordination
 *   (rope for platforms) to degraded performance (piton for traditional
 *   verification institutions) to organized alternatives with sunset paths
 *   (scaffold for epistemic defense coalitions) to naturalized inevitability
 *   (mountain from the analytical civilizational view, falsely). The
 *   constraint's base extractiveness has nearly doubled over the 20-year
 *   interval (0.28 → 0.58) as platforms optimized ranking algorithms for
 *   engagement metrics, and theater ratio has nearly doubled (0.45 → 0.81) as
 *   traditional fact-checking institutions responded with increasingly
 *   elaborate verification rituals that have minimal impact on what actually
 *   spreads. The constraint is most accurately classified as a snare: it
 *   extracts truth-alignment capacity from epistemic agents, depends on
 *   suppressing reality-checking alternatives, and persists through network
 *   effects and platform lock-in rather than through genuine coordination
 *   benefits.
 *
 * KEY AGENTS:
 *   - Attention-Extraction Platforms: Primary beneficiary (institutional/arbitrage) — profit from engagement metrics and advertising revenue regardless of truth value; directly benefit from memetic distortion
 *   - Engagement-Incentive Actors: Secondary beneficiary (institutional/arbitrage) — advertisers, creators, political actors who benefit from virality independent of accuracy; aligned with platform incentives
 *   - Epistemic Commons: Primary victim (powerless/trapped) — shared basis for collective reality assessment contaminated and degraded; no exit mechanism
 *   - Individual Information Consumers: Secondary victim (powerless/trapped) — cognitive resources captured by false signals; no realistic exit from digital information environment
 *   - Reality-Grounded Knowledge Communities: Tertiary victim (moderate/constrained) — face suppression through memetic competition; benefit from collaborative knowledge infrastructure but bear burden of constant correction
 *   - Traditional Verification Institutions: Institutional actor in decline (institutional/arbitrage) — fact-checkers, journals, news organizations maintain performative verification rituals; their authority is assumed but their practical influence on viral spread is marginal
 *   - Organized Epistemic Defense Coalition: Emerging alternative actors (organized/constrained) — prediction markets, epistemic wikis, crowdsourced fact-checking, open science initiatives building parallel truth-ranking mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(memetic_fitness_vs_truth, 0.58).
domain_priors:suppression_score(memetic_fitness_vs_truth, 0.68).
domain_priors:theater_ratio(memetic_fitness_vs_truth, 0.81).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, extractiveness, 0.58).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(memetic_fitness_vs_truth, theater_ratio, 0.81).

% --- Constraint claim ---
narrative_ontology:constraint_claim(memetic_fitness_vs_truth, snare).
narrative_ontology:human_readable(memetic_fitness_vs_truth, "The Viral Distortion: Memetic Fitness Decoupling from Truth").
narrative_ontology:topic_domain(memetic_fitness_vs_truth, "social/informational/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(memetic_fitness_vs_truth, attention_extraction_platforms).
narrative_ontology:constraint_beneficiary(memetic_fitness_vs_truth, engagement_incentive_actors).
narrative_ontology:constraint_victim(memetic_fitness_vs_truth, epistemic_commons).
narrative_ontology:constraint_victim(memetic_fitness_vs_truth, reality_grounded_information).
narrative_ontology:constraint_victim(memetic_fitness_vs_truth, individual_judgment_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — Cannot exit the viral substrate; bears full cost of memetic distortion through contaminated information ecosystem. The shared basis for collective reality assessment has no advocate and no defection mechanism. Maximum experienced extraction — abstract epistemic good cannot organize or protect itself.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL INFORMATION CONSUMER (SNARE) — Trapped in an attention economy where cognitive resources are finite but false signals are optimized to capture them. No realistic exit option from the digital information environment. Bears extraction through misallocation of attention and cognitive burden of navigating distorted information landscape.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REALITY-GROUNDED KNOWLEDGE COMMUNITY (TANGLED ROPE) — Scientists, investigators, and truth-seekers benefit from collaborative knowledge-sharing infrastructure (journals, conferences, databases) but face suppression through memetic competition with simpler, more attention-grabbing false narratives. Significant extraction through burden of constant correction and diminished credibility ceiling, but also genuine coordination benefits from shared epistemic methods.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ATTENTION-EXTRACTION PLATFORMS (ROPE) — Experience the memetic fitness constraint as pure coordination problem: their business model solves the problem of matching content to human attention. They derive benefit from the decoupling (fitness-based ranking is simpler and more profitable than truth-based ranking). Experiences constraint as coordination mechanism; extraction benefits them directly.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: TRADITIONAL MEDIA & FACT-CHECKING (PITON) — Established gatekeepers (news organizations, academic journals, fact-check services) maintain verification rituals that are substantially performative. Their editorial review processes are theater relative to their actual capacity to influence what spreads virally. The apparatus persists through institutional legitimacy (assumed authority) rather than functional effectiveness — their fact-checks spread at a fraction of the false claims they debunk. Theater ratio high (0.81) reflects this decoupling.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ORGANIZED EPISTEMIC DEFENSE COALITION (SCAFFOLD) — Decentralized communities (epistemic wikis, crowdsourced fact-checking, prediction markets, open science initiatives) represent emerging alternative verification pathways with sunset logic. These actors see the memetic distortion as a coordination failure solvable through distributed truth-seeking mechanisms. Experiences extraction as temporary — the constraint's force will decline as alternative ranking systems (credibility, prediction accuracy, citation impact in specialized communities) mature and compete with pure engagement metrics.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION THERMODYNAMICS (MOUNTAIN) — From a civilizational perspective, some degree of memetic distortion may be inherent to information systems with finite bandwidth and unbounded ideation: simpler ideas always propagate faster than complex ones, and any system that surfaces ideas to human attention will select for emotional resonance over accuracy. This perspective sees the decoupling as a natural law of information dynamics. However, the structural data (high suppression, institutional enforcement, platform agency) contradicts pure naturalization — the engine will flag this as a false summit.
constraint_indexing:constraint_classification(memetic_fitness_vs_truth, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(memetic_fitness_vs_truth_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(memetic_fitness_vs_truth, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(memetic_fitness_vs_truth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(memetic_fitness_vs_truth, TR),
    TR >= 0.70.

:- end_tests(memetic_fitness_vs_truth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate and increasing. The platforms extract value from epistemic agents by prioritizing engagement over accuracy, creating informational asymmetries that benefit attention-extraction businesses. The base value is not as extreme as pure snares (0.70+) because some coordination value exists — the platforms do genuinely solve the problem of matching abundant content to scarce human attention. However, the solution is decoupled from truth alignment, and the extraction is asymmetric: platforms gain while the epistemic commons loses. The 20-year trajectory (0.28 → 0.58) reflects accelerating optimization of ranking algorithms for engagement metrics as competition intensified. Suppression (0.68): High and structural. The barriers to reality-grounded information spreading virally include: cognitive limitation (simpler narratives spread faster), emotional logic (false claims often trigger stronger affective responses), network effects (existing popularity breeds more popularity regardless of truth), algorithmic amplification (engagement metrics select for viral properties, not accuracy), and coordinated promotion (false claims are often amplified by organized actors with incentive to spread them). These barriers are not easily overcome through individual effort. Theater ratio (0.81): Very high and increasing. Traditional fact-checking institutions perform increasingly elaborate verification rituals (detailed studies, peer review, media fact-check articles) but their output spreads at a tiny fraction of the false claims they address. This is pure theater: the ritual of verification has decoupled from its functional impact. As platforms have optimized for engagement, fact-checkers have responded by elaborating their verification methods, but the gap between their output's virality and false claims' virality has widened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates marked perspectival divergence. The platform experiences it as a coordination problem that they have elegantly solved: matching unlimited content to limited attention is a real engineering challenge, and engagement metrics are an effective solution. Their genuine experience is rope-like coordination. The powerless consumer experiences pure extraction: their attention is captured by false signals, their judgment is distorted, and they have no means to exit. Their genuine experience is snare. The epistemic commons (an abstract collective) experiences elimination: the shared basis for reality assessment has been contaminated. Traditional fact-checkers experience their own degradation: they maintain verification rituals that have become increasingly performative and decreasingly influential. The organized epistemic coalition experiences a temporary problem with a sunset: alternative platforms and ranking mechanisms are being built that will eventually provide substitutes for engagement-based ranking. Each perspective reflects a structurally accurate reading of their position relative to the constraint; the gap between them reveals the asymmetries in the system.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the memetic fitness constraint. Platforms with arbitrage options experience low d (0.10-0.20): they are net beneficiaries whose business models are aligned with memetic distortion. Powerless consumers with no exit experience high d (0.90-0.95): they bear extraction costs through misallocated attention and degraded reality models. Reality-grounded communities experience moderate-high d (0.60-0.70): they both benefit from epistemic collaboration and bear costs from memetic suppression. Traditional institutions experience low d with high theater (0.20-0.30 but piton classification): they are assumed beneficiaries but their functional influence has degraded. Organized alternatives experience moderate d (0.45-0.55): they see exit paths (alternative ranking mechanisms) and have agency to build them, so experienced extraction is moderate rather than maximal.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY CRUX: The memetic fitness versus truth constraint resolves the mandatrophy by exposing the institutional choice embedded in the 'natural law' interpretation. The analytical observer's mountain perspective (memetic distortion is inherent to information systems) is a FALSE SUMMIT. The structural data reveals that this is not an immutable property of information but a contingent choice by platform institutions to optimize for engagement metrics rather than accuracy. Counterfactual platforms exist (epistemic wikis, specialized communities with curated ranking, prediction markets) that operate with higher truth-alignment. The platforms' rope experience is genuine coordination value — they do solve a real attention-matching problem. But that coordination value does not require engagement-based ranking; it requires content-to-user matching, which could be done via accuracy, credibility, predictive power, or specialized domain relevance. The constraint's extractiveness (0.58) reflects the contingent choice to optimize for engagement as the ranking criterion, not an inevitable consequence of matching content to attention. If the choice is institutional rather than structural, then the snare classification is correct: this is extraction enabled by platform power and normalized as natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fitness_truth_decoupling_threshold,
    'At what degree of virality decoupling do false claims systematically outcompete truth-aligned claims in the informational substrate?',
    'Empirical analysis of viral curves: measure time-to-peak and sustained reach for claims verified as true vs false across multiple information domains; correlation analysis of virality metrics with truth-value assessments',
    'If decoupling occurs at engagement ratios < 3:1 (false:truth): epistemic filtering is still viable. If > 5:1: snare classification is robust. If >> 10:1: false claims have effectively captured the substrate, and mountain classification becomes plausible (structural inevitability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_truth_decoupling_threshold, empirical, 'Threshold where false claims systematically outcompete truth in virality').

omega_variable(
    platform_algorithm_malice_vs_design,
    'Does the memetic distortion result from deliberate optimization of platforms for engagement (institutional choice) or from inevitable properties of attention-ranking systems (structural necessity)?',
    'Historical analysis of platform design choices: counterfactual assessment of what ranking algorithms would look like if optimized for accuracy rather than engagement; comparison of truth-aligned platforms (e.g., Epistemic Status on some wikis) with engagement-optimized platforms; controlled experiments with alternative ranking functions',
    'If deliberate: snare classification stands — platforms are active extractors. If inevitable: classification shifts toward tangled_rope or piton — platforms are solving a genuine coordination problem (matching content to attention) that inherently decouples from truth. This is the mandatrophy crux.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_algorithm_malice_vs_design, conceptual, 'Whether platform distortion is deliberate or structural inevitability').

omega_variable(
    alternative_ranking_viability,
    'Can decentralized truth-ranking mechanisms (prediction markets, credibility scoring, open epistemic ontologies) actually compete with engagement-optimized ranking at scale?',
    'Longitudinal comparison of alternative platform growth and user adoption rates; measurement of virality differential when same claims are presented through both engagement-ranking and credibility-ranking contexts; analysis of whether credibility-ranked platforms can achieve network effects sufficient to challenge engagement-ranked incumbents',
    'If viable: scaffold perspective confirmed — sunset of traditional memetic distortion is real. If unviable: the constraint is structural (mountain) or permanently extractive (snare). This determines whether the present state is transitional or stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_ranking_viability, empirical, 'Whether truth-aligned ranking mechanisms can compete at scale').

omega_variable(
    human_attention_optimization_surface,
    'Do false claims gain virality primarily through legitimate attention capture (emotional resonance, narrative coherence) or through dark patterns (deception, manipulation, coordinated amplification)?',
    'Content analysis: deconstruct viral false claims into components (emotional triggers, narrative structure, false evidence, coordinated promotion); measure each component''s contribution to virality; isolate legitimate vs manipulative factors in comparative virality analysis',
    'If primarily legitimate: the snare is soft (agents could in principle exit by training attention-management skills); piton classification is defensible (people *choose* to share false claims). If primarily dark patterns: the snare is hard (agents cannot realistically protect themselves); suppression is structural, not behavioral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(human_attention_optimization_surface, empirical, 'Whether false virality stems from legitimate or manipulative mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(memetic_fitness_vs_truth, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meme_tr_t0, memetic_fitness_vs_truth, theater_ratio, 0, 0.45).
narrative_ontology:measurement(meme_tr_t10, memetic_fitness_vs_truth, theater_ratio, 10, 0.64).
narrative_ontology:measurement(meme_tr_t20, memetic_fitness_vs_truth, theater_ratio, 20, 0.81).

% Extraction over time
narrative_ontology:measurement(meme_be_t0, memetic_fitness_vs_truth, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(meme_be_t10, memetic_fitness_vs_truth, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(meme_be_t20, memetic_fitness_vs_truth, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(memetic_fitness_vs_truth, information_standard).
narrative_ontology:affects_constraint(memetic_fitness_vs_truth, misinformation_production_incentive).
narrative_ontology:affects_constraint(memetic_fitness_vs_truth, attention_economy_extraction).
narrative_ontology:affects_constraint(memetic_fitness_vs_truth, epistemic_authority_degradation).

% DUAL FORMULATION NOTE:
% The memetic fitness constraint is upstream of specific misinformation production (particular false claims) and attention-economy extraction (specific attention-capture mechanisms). The upstream constraint (this story) describes the structural decoupling between virality and truth; downstream constraints address how particular actors exploit or resist that decoupling. All three are linked: they share platforms and attention substrates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(memetic_fitness_vs_truth, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
