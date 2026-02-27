% ============================================================================
% CONSTRAINT STORY: attention_as_bottleneck_resource
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_attention_as_bottleneck_resource, []).

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
 *   constraint_id: attention_as_bottleneck_resource
 *   human_readable: The Finite Cognitive Aperture: Attention as Bottleneck Resource
 *   domain: social/technological/economic
 *
 * SUMMARY:
 *   The finite cognitive aperture of human attention has become the primary
 *   bottleneck for social coordination, knowledge production, and economic
 *   value capture in information-dense societies. As the quantity of
 *   available information, entertainment, and social signals has expanded
 *   exponentially (driven by digital technology, globalization, and real-time
 *   content production), the biological limit of human attentional capacity
 *   has remained constant. This mismatch creates a structural constraint
 *   where attention itself becomes a scarce resource, and institutional
 *   mechanisms (platforms, algorithms, media systems) compete to control its
 *   allocation. The constraint exhibits the full range of DR types depending
 *   on observer position: platforms and high-status signal-holders experience
 *   it as Rope (pure coordination of a shared resource); attention-starved
 *   populations and the epistemic commons experience it as Snare (pure
 *   extraction); attention-constrained professionals experience it as Tangled
 *   Rope (mixed coordination and extraction); alternative governance
 *   coalitions experience it as Scaffold (temporary problem with
 *   institutional sunset); legacy media experience it as Piton (degraded
 *   coordination mechanism maintained by inertia); and the analytical
 *   observer risks naturalizing the institutional amplification as an
 *   immutable biological law.
 *
 * KEY AGENTS:
 *   - Attention-Extraction Platforms: Institutional beneficiary (institutional/arbitrage) — capture user engagement, behavioral data, and economic value from attention routing; experience constraint as pure coordination enabling their service
 *   - High-Signal Status Holders: Secondary beneficiary (powerful/arbitrage) — celebrities, influencers, established institutions benefit from attention-allocation mechanisms that privilege existing status and novelty; can command attention through brand/reputation
 *   - Attention-Starved Populations: Primary victim (powerless/trapped) — individuals and communities with low institutional visibility, limited access to signal-production infrastructure, or marginalized social position cannot acquire attention for legitimate claims or contributions; no exit option from mandatory information economy
 *   - Attention-Constrained Professionals: Secondary victim (moderate/constrained) — researchers, educators, domain experts must compete for attention despite having valuable but complex signals; lose productive time to engagement optimization; constrained exit (must participate in attention markets to maintain professional influence)
 *   - Marginal Signal Producers: Tertiary victim (powerless/mobile) — creators of specialized, niche, or unpolished content lose attention competition to entertainment and status-signaling content despite potential value; mobile exit (can retreat from platforms, but social/economic cost is high)
 *   - Epistemic Commons: Structural victim (analytical/trapped) — abstract collective good of shared knowledge quality and mutual understanding; contaminated by low-signal noise; cannot organize or defend itself; trapped in system it does not control
 *   - Attention-Governance Coalition: Organized agent (organized/constrained) — regulators, digital-rights advocates, alternative-platform builders, labor standards advocates; constrained exit (cannot fully escape legacy platforms but building parallel systems); see architectural sunset (changes in algorithms, governance, regulation)
 *   - Legacy Attention-Distribution System: Institutional actor (institutional/arbitrage) — newspapers, broadcasting, publishing houses; maintain attention control through perceived authority and archive access despite functional atrophy; arbitrage exit (can adapt or fold)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the institutional design as immutable biological limit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(attention_as_bottleneck_resource, 0.52).
domain_priors:suppression_score(attention_as_bottleneck_resource, 0.68).
domain_priors:theater_ratio(attention_as_bottleneck_resource, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, extractiveness, 0.52).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(attention_as_bottleneck_resource, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(attention_as_bottleneck_resource, tangled_rope).
narrative_ontology:human_readable(attention_as_bottleneck_resource, "The Finite Cognitive Aperture: Attention as Bottleneck Resource").
narrative_ontology:topic_domain(attention_as_bottleneck_resource, "social/technological/economic").

domain_priors:requires_active_enforcement(attention_as_bottleneck_resource).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, attention_extraction_platforms).
narrative_ontology:constraint_beneficiary(attention_as_bottleneck_resource, high_signal_status_holders).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, attention_deficit_populations).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, epistemic_commons).
narrative_ontology:constraint_victim(attention_as_bottleneck_resource, marginal_signal_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATTENTION-STARVED POPULATIONS (SNARE) — Individuals and communities with marginal social capital, institutional access, or signal-production capacity cannot acquire attention for legitimate claims, needs, or contributions. No exit option exists: the global attention economy is mandatory for survival in information-dense societies. These agents bear maximum cost while extractors capture their cognitive labor (data, engagement, derived attention). The constraint appears as pure extraction from this vantage.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ATTENTION-CONSTRAINED PROFESSIONALS (TANGLED ROPE) — Knowledge workers, educators, researchers, and domain experts face asymmetric extraction: they must compete for attention to maintain influence and funding, but the competition is rigged in favor of high-signal novelty and entertainment value over depth or accuracy. They benefit from attention-seeking mechanisms (publishing, speaking, engagement) but also lose massive amounts of productive time to the competition. Mixed coordination (the system does enable legitimate signal distribution) and extraction (the system selects for engagement over truth).
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ATTENTION-EXTRACTION PLATFORMS (ROPE) — Tech platforms (social media, search engines, streaming services, recommendation algorithms) experience the constraint as a pure coordination mechanism. They are solving the legitimate problem of routing finite user attention to content. The constraint enables their business model and benefits them directly through data extraction and engagement capture. They see the bottleneck as an opportunity, not a burden. Arbitrage exit (can always pivot to other business models) and primary beneficiary status.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ATTENTION-GOVERNANCE COALITION (SCAFFOLD) — Regulatory bodies, labor standards advocates, digital-rights organizations, and alternative-platform builders (Bluesky, Mastodon, cooperative social networks) view the attention bottleneck as a temporary coordination failure with a sunset. They are building alternative governance structures (algorithmic transparency, attention-preserving regulations, open-source platforms) to lower the extraction rate and distribute attention-control more equitably. The sunset is conditional on regulatory adoption and alternative platform maturation — estimated 10-20 years for meaningful norm shifts in dominant platforms.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ATTENTION-DISTRIBUTION SYSTEM (PITON) — Traditional media (newspapers, broadcasting, publishing houses) formerly controlled attention routing. They now persist as a parallel system maintained through institutional inertia (prestige effects, regulatory deference, archive access) despite their functional atrophying. Many legacy institutions capture attention through nostalgia and perceived trustworthiness, not through adaptive coordination. Theater ratio is high: editorial review appears to add value, but algorithmic curation on platforms often has superior tracking of actual user interest.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / BIOLOGICAL LIMIT VIEW (MOUNTAIN) — From a universal/civilizational perspective, human attention is a finite biological resource with immutable constraints. No individual can attend to more than ~7±2 items simultaneously, and daily focus capacity is bounded by circadian cycles and cognitive fatigue. This constraint is fundamental to the human organism itself — no institutional arrangement can alter the underlying limit. However, the engine's false summit detector will identify this as naturalization: the constraint's extractiveness and suppression scores reflect institutional design choices (recommendation algorithms, engagement metrics, attention markets), not biological limits. The biological aperture is real; the institutional amplification of its coercive effect is contingent.
constraint_indexing:constraint_classification(attention_as_bottleneck_resource, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(attention_as_bottleneck_resource_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(attention_as_bottleneck_resource, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(attention_as_bottleneck_resource, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(attention_as_bottleneck_resource, TR),
    TR >= 0.70.

:- end_tests(attention_as_bottleneck_resource_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The constraint exhibits significant asymmetric extraction: platforms capture economic value from user attention; high-status holders benefit from existing attention-routing mechanisms; attention-starved populations lose the cognitive labor of signal production without receiving attention or compensation. The extractiveness increased over the interval (0.28→0.52) as algorithmic amplification replaced human curation, creating path-dependent lock-in where engagement metrics replaced quality signals. Not at the 0.66 threshold for pure Snare because coordination functions are real — platforms do solve the problem of routing attention in large information spaces — and some agents (platforms, established institutions) see net benefits. Suppression (0.68): High. Significant barriers to exit and alternative signal routes include: platform network effects (attention follows users, users follow attention), algorithmic opacity, advertising dependency, data lock-in, career/social cost of platform exit, lack of viable alternative infrastructure for most users. Suppression is lower than pure-Snare levels (≥0.60) only because some exit options exist (alternative platforms, attention discipline, niche communities) even though they carry high cost. Theater ratio (0.61): Moderate-high. Engagement metrics (likes, shares, comments, watch time) are performative proxies for signal value — optimizing for engagement selects for entertainment and sensation over truth or utility. Editorial curation has lower theater than algorithmic feeds, but even editorial systems optimize for reader retention. The theater has increased over the interval (0.35→0.61) as platforms replaced editorial curation with engagement-maximizing algorithms. Open-governance alternatives (Wikipedia, academic peer review, trusted expert systems) achieve lower theater (~0.30-0.45) by decoupling signal routing from engagement metrics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Platforms experience pure Rope (solving the coordination problem of routing scarce attention). Attention-starved populations experience pure Snare (trapped extraction). Professionals experience Tangled Rope (mixed coordination benefits and extraction costs). Governance coalitions experience Scaffold (sunset pathway through regulation and alternative platforms). Legacy media experience Piton (performative function maintained by inertia). The analytical observer risks Mountain (naturalizing institutional design as biological law). The gap reveals how the same structural phenomenon — finite human cognitive capacity — is experienced as solution, extraction, mixed system, temporary failure, degradation, or immutable law depending on the agent's structural position. No single perspective is 'correct'; the presheaf over observation sites is the diagnosis.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are computed from beneficiary/victim status and exit options. Platform actors as institutional beneficiaries with arbitrage exits (can pivot business models) derive low d values (~0.05-0.15), experiencing negative or near-zero effective extraction — the constraint benefits them. Powerless attention-starved populations with trapped exits derive high d values (~0.90-0.95), experiencing maximum effective extraction — the constraint extracts from them. Moderate professionals with constrained exits (must participate in attention markets to maintain influence) derive moderate d values (~0.55-0.65), experiencing moderate extraction. Organized coalitions with constrained exits but agency in building alternatives derive lower-moderate d values (~0.40-0.50), experiencing moderate extraction but with path toward reduction. The analytical observer with analytical exit derives moderate d (~0.72), experiencing moderate extraction due to difficulty of evaluating the full system from within it.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by revealing the distinction between biological limits and institutional amplification. The biological limit (human attention is finite) is real and immutable — this is a genuine 'mountain' fact about cognition. However, the extractiveness (0.52) and suppression (0.68) scores reflect institutional choices: engagement-maximizing algorithms, platform network effects, attention-market competition, and status-quo bias toward established signal-holders. These are Tangled Rope / Snare properties, not Mountain properties. The false summit detector will flag the analytical observer's Mountain classification as mislabeled — the institutional amplification is contingent, not lawlike. The true classification is Tangled Rope across most perspectives: real coordination function (routing attention in large information spaces) mixed with real extraction (capture of attention for low-utility signals, asymmetric benefits to platforms and high-status holders). The biological aperture sets the floor; institutional design sets the ceiling and internal allocation. Reforming the institutions (transparency in curation, decoupling from engagement metrics, alternative platforms, algorithmic governance) could reduce extractiveness to 0.25-0.35 (moving toward Rope) while maintaining coordination function. This is not a Mountain because the outcome is sensitive to institutional choice.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_vs_biological_ceiling,
    'Is the observed attention bottleneck a consequence of fixed biological limits or of institutional design choices that artificially amplify scarcity?',
    'Comparative analysis of attention-distribution outcomes under different institutional arrangements (algorithmic vs human curation, attention-preserving vs engagement-maximizing metrics). Historical data from periods before algorithmic amplification (pre-1990s media attention distribution).',
    'If biological dominates: constraint is closer to Mountain (suppression and extractiveness are structural features of cognition). If institutional dominates: constraint is Tangled Rope or Snare (suppression and extractiveness are governance choices, potentially reformable).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_vs_biological_ceiling, empirical, 'Whether bottleneck is biological limit or institutional amplification').

omega_variable(
    attention_fungibility_across_domains,
    'Can attention devoted to entertainment or social connection meaningfully substitute for attention to civic participation, skill development, or epistemic collaboration?',
    'Cognitive psychology studies on attention allocation; behavioral analysis of individuals'' attention budgets across domains; whether time spent on entertainment directly reduces time spent on other domains or whether it is independently allocated.',
    'If fungible: the constraint is coordination (shared resource, but not necessarily zero-sum extraction). If non-fungible: different attention pools have different extraction dynamics, requiring separate constraint stories for civic vs entertainment vs epistemic domains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attention_fungibility_across_domains, empirical, 'Whether attention is fungible across social domains').

omega_variable(
    collective_vs_individual_aperture,
    'Does institutional coordination (delegation, expertise, trusted intermediaries) effectively expand the collective cognitive aperture beyond the sum of individual limits?',
    'Comparative analysis of collective decision-making quality with vs without institutional mediation (Wikipedia vs random crowd, peer review vs open review, editorial curation vs algorithmic feed). Measurement of information quality per unit of individual cognitive input.',
    'If expansion is real: constraint is pure Rope (coordination solves scarcity). If expansion fails: constraint is Tangled Rope or Snare (institutional mediation is itself extractive and fails to solve the underlying problem).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_vs_individual_aperture, empirical, 'Whether institutional coordination expands collective cognitive aperture').

omega_variable(
    attention_market_equilibrium,
    'Does competition for attention in a market system produce equilibrium that distributes signal by quality, or does it systematically favor sensation, status signaling, and entertainment value over truth and utility?',
    'Content analysis: comparison of signal-to-noise ratios in different attention-allocation systems (algorithmic feeds, human-curated, democratic voting, expertise-weighted). Measurement of correlation between attention received and actual utility/accuracy of information.',
    'If equilibrium is quality-biased: market mechanisms are Rope (coordination). If equilibrium is sensation-biased: market mechanisms are Snare (extraction of attention toward low-utility signals).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_market_equilibrium, empirical, 'Whether attention markets equilibrate around quality or sensation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(attention_as_bottleneck_resource, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(attn_tr_t0, attention_as_bottleneck_resource, theater_ratio, 0, 0.35).
narrative_ontology:measurement(attn_tr_t15, attention_as_bottleneck_resource, theater_ratio, 15, 0.48).
narrative_ontology:measurement(attn_tr_t30, attention_as_bottleneck_resource, theater_ratio, 30, 0.61).

% Extraction over time
narrative_ontology:measurement(attn_be_t0, attention_as_bottleneck_resource, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(attn_be_t15, attention_as_bottleneck_resource, base_extractiveness, 15, 0.4).
narrative_ontology:measurement(attn_be_t30, attention_as_bottleneck_resource, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(attention_as_bottleneck_resource, information_standard).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, epistemic_commons_contamination).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, status_market_fragmentation).
narrative_ontology:affects_constraint(attention_as_bottleneck_resource, algorithmic_curation_lock_in).

% DUAL FORMULATION NOTE:
% The finite cognitive aperture decomposes into three structurally distinct constraints: (1) the biological limit of individual attention (Mountain-adjacent but not included here), (2) the institutional amplification through engagement-metric optimization (this story: Tangled Rope), and (3) the network effect dynamics that lock users into platform ecosystems (separate story: affects_constraints). Each has different extractiveness and different institutional reform pathways. This story focuses on the core tension between biological scarcity and institutional extraction mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(attention_as_bottleneck_resource, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
