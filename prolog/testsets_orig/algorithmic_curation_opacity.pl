% ============================================================================
% CONSTRAINT STORY: algorithmic_curation_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_curation_opacity, []).

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
 *   constraint_id: algorithmic_curation_opacity
 *   human_readable: Algorithmic Curation Opacity in Content Distribution Platforms
 *   domain: digital_platform_governance/information_asymmetry
 *
 * SUMMARY:
 *   Algorithmic curation opacity in major content distribution platforms
 *   represents a structurally dual constraint: it simultaneously solves
 *   genuine coordination problems (preventing spam, gaming, adversarial
 *   manipulation) and enables extractive asymmetries (capturing user
 *   attention and behavioral data without transparency or consent). The
 *   opacity is not incidental to the platform's function — it is central to
 *   how the platform achieves scale. Yet the opacity also prevents users and
 *   creators from understanding how their content is ranked, why certain
 *   information is visible or hidden, and what behavioral data is collected.
 *   The constraint exhibits Tangled Rope structure: beneficiaries (platform
 *   operators) maintain the opacity through active enforcement of algorithmic
 *   secrecy; victims (content creators, users, information quality) are
 *   trapped by network effects and switching costs; some agents (advertisers,
 *   regulators) experience mixed extraction and coordination. The theater
 *   ratio (0.65) reflects that platforms increasingly make public claims
 *   about algorithmic fairness and transparency while maintaining operational
 *   opacity — moderation is performed as a ritual rather than executed
 *   transparently. The extractiveness trajectory (0.35 → 0.58 over the
 *   interval) shows increasing extraction as platforms have become more
 *   centralized, more reliant on engagement-maximization algorithms, and more
 *   resistant to transparency demands.
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — control algorithmic ranking and capture user attention; benefit from opacity because transparency would reduce flexibility for engagement optimization
 *   - Content Creators: Primary victim (powerless/trapped) — depend on algorithmic distribution; cannot inspect ranking mechanisms; forced to optimize for opaque algorithmic preferences rather than substantive quality
 *   - Platform Users: Primary victim (powerless/trapped) — trapped by network effects and social graphs; manipulated by opaque algorithmic ranking; cannot make informed choices about information diet
 *   - Information Quality: Systemic victim (powerless/trapped) — abstract collective good; damaged by creator incentives to optimize for algorithmic engagement rather than truth value; no mechanism for correction
 *   - Advertisers: Secondary beneficiary/victim (moderate/constrained) — benefit from opaque targeting but constrained by inability to inspect or predict algorithmic behavior
 *   - Regulatory Coalition: Organized agent (organized/constrained) — EU DSA, AI Act, digital rights advocates building regulatory pressure for transparency; sees opacity as temporary problem with sunset clause
 *   - Content Moderation System: Institutional actor (institutional/arbitrage) — maintains moderation theater; relies on algorithmic opacity to avoid revealing inconsistencies and biases in enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_curation_opacity, 0.58).
domain_priors:suppression_score(algorithmic_curation_opacity, 0.68).
domain_priors:theater_ratio(algorithmic_curation_opacity, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_curation_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_curation_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_curation_opacity, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_curation_opacity, tangled_rope).
narrative_ontology:human_readable(algorithmic_curation_opacity, "Algorithmic Curation Opacity in Content Distribution Platforms").
narrative_ontology:topic_domain(algorithmic_curation_opacity, "digital_platform_governance/information_asymmetry").

domain_priors:requires_active_enforcement(algorithmic_curation_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_curation_opacity, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_curation_opacity, algorithmic_optimization_incentives).
narrative_ontology:constraint_victim(algorithmic_curation_opacity, content_creators).
narrative_ontology:constraint_victim(algorithmic_curation_opacity, platform_users).
narrative_ontology:constraint_victim(algorithmic_curation_opacity, information_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONTENT CREATOR (SNARE) — Trapped by platform dependence for audience reach. No viable alternative with equivalent distribution. Extraction mechanism: algorithm penalizes content that doesn't maximize engagement metrics; creators optimize for algorithmic preferences rather than substantive value. Exit barrier is the network effect — leaving means losing accumulated audience and algorithmic training. Suppression operates through opacity: creators cannot inspect how their content is ranked, cannot predict algorithmic preferences reliably, and cannot exit without catastrophic audience loss.
constraint_indexing:constraint_classification(algorithmic_curation_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: USER / INFORMATION CONSUMER (SNARE) — Trapped by network effects and switching costs. The platform's social graph, content library, and recommendation infrastructure make exit prohibitively costly. Extraction mechanism: users bear the cognitive cost of navigating opacity while platform captures behavioral data and attention. Suppression is high: algorithmic ranking is not transparent, users cannot predict what they will see, and cannot exercise meaningful choice without leaving the platform entirely.
constraint_indexing:constraint_classification(algorithmic_curation_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences opacity as pure coordination mechanism. Algorithm must be opaque to prevent gaming and manipulation. The opacity constraint coordinates competing goals: maximizing engagement, preventing spam, controlling viral misinformation, and maintaining platform stability. From the platform's structural position (institutional power, arbitrage exit — could theoretically disclose algorithms but chooses not to), the constraint appears as necessary coordination. Net beneficiary position allows this operator to see the constraint as serving legitimate technical functions rather than extractive.
constraint_indexing:constraint_classification(algorithmic_curation_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER (TANGLED ROPE) — Partially constrained but also partially enabled by opacity. Benefits from algorithmic targeting that reaches specific audience segments; constrained by inability to inspect why ads show to certain users or predict algorithmic performance. Some leverage through budget control (can arbitrage between platforms), but significant extraction through algorithmic opacity: advertisers pay premiums for opaque targeting, cannot fully verify ROI, and face algorithmic changes that destabilize campaigns. Mixed experience of coordination (audience selection works) and extraction (opaque metrics, performance variability).
constraint_indexing:constraint_classification(algorithmic_curation_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY COALITION (SCAFFOLD) — Organized agents (EU regulators, digital rights advocates, open-source protocol developers) view algorithmic opacity as a temporary structural problem with a sunset clause. Regulation (DSA, AI Act), technical standards (open recommendation systems), and decentralized alternatives are creating pathways toward transparency and interoperability. Constraint sees itself as time-bounded: as regulatory pressure increases and alternative platforms emerge, the opacity-extraction mechanism loses force. Low effective extraction from regulatory perspective because this agent has institutional power and sees clear exit paths (mandated algorithm transparency, forced interoperability, alternative platforms).
constraint_indexing:constraint_classification(algorithmic_curation_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT MODERATION THEATER (PITON) — Platform moderation relies substantially on algorithmic opacity as a performative mechanism: platforms claim to have 'policies' and 'standards' but enforce them through opaque algorithms that operators themselves do not fully understand (machine-learning black boxes). Theater ratio (0.65) reflects that moderation claims far exceed actual transparency or consistency. The institutional system maintains the theater through inertia — full transparency would expose inconsistency and bias, so opacity is maintained as protective fiction. Operators have arbitrage exit (could theoretically increase transparency) but maintain opacity to preserve the moderation narrative.
constraint_indexing:constraint_classification(algorithmic_curation_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, algorithmic opacity serves two structurally distinct functions: genuine coordination (preventing gaming, spam, adversarial manipulation) and extractive asymmetry (capturing user attention and behavioral data without consent or compensation). The constraint is not purely coordination because beneficiaries extract rent from opacity, nor is it pure extraction because some opacity does solve real coordination problems. The classification remains tangled rope: both functions are real, both require enforcement, and the opacity mechanism enables both simultaneously. The challenge for analysis is decomposing which portion serves which function.
constraint_indexing:constraint_classification(algorithmic_curation_opacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_curation_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_curation_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_curation_opacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_curation_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_curation_opacity, TR),
    TR >= 0.70.

:- end_tests(algorithmic_curation_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform captures significant value through algorithmic opacity: attention capture, behavioral data extraction, ability to manipulate information flow for engagement metrics or advertising revenue. The extraction is not maximal (0.70+) because some platforms do allow limited transparency features, users retain basic agency (can search, follow specific accounts), and regulatory pressure is creating compliance costs. The increase from 0.35 to 0.58 reflects consolidation of platforms (fewer competitors), increased sophistication of engagement-optimization algorithms, and accumulation of behavioral data assets that make platforms more extractive. Suppression (0.68): High. The mechanisms maintaining opacity are substantial: (1) algorithmic complexity makes true transparency technically difficult or costly, (2) platforms claim business secret protection for algorithms, (3) users face high exit costs (network effects, data portability friction), (4) creators have no viable alternative platforms at scale, (5) information about algorithmic behavior is deliberately restricted. Theater ratio (0.65): High and increasing. Platform claims about 'algorithmic fairness', 'content moderation standards', and 'transparency reports' far exceed operational reality. The gap between public transparency commitments and actual opacity suggests theater is central to the constraint — platforms perform compliance and fairness without restructuring the opacity that enables extraction.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. The platform operator sees a coordination mechanism (Rope) — algorithmic opacity prevents gaming and enables technical function at scale. The content creator sees pure extraction (Snare) — they are trapped by algorithmic dependence and cannot inspect the ranking mechanism. The user sees extraction masked as curation (Snare) — algorithmic filtering of their information diet is not transparent. The advertiser sees mixed extraction and coordination (Tangled Rope) — they benefit from algorithmic targeting but are constrained by opacity in campaign optimization. The regulatory coalition sees a sunset problem (Scaffold) — DSA transparency requirements, AI Act algorithmic auditing, and alternative platforms are building pathways to reduced opacity. The moderation system sees theater (Piton) — claims of fairness and standards persist through opacity, not through actual consistent implementation. The analytical observer sees Tangled Rope with high mandatrophy risk — distinguishing the genuine coordination function from extractive asymmetry requires decomposing which portion of opacity serves which purpose.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) follow from structural positions: platform operators are beneficiaries with arbitrage options (low d, negative chi experienced as 'coordination works'), content creators and users are victims with trapped exit options (high d, high chi experienced as extraction). Advertisers are ambiguous (benefit from targeting but constrained by opacity, moderate d). The regulatory coalition has organized power and sees clear exits (regulatory mandates, alternative platforms, reduced d). The moderation system appears institutional but is captured by operator interests (low d from operator perspective, but this is misleading — the moderation system is serving extractive rather than truth-seeking functions). The analytical perspective integrates all positions and recognizes that d varies by agent even within the same constraint: the same opacity mechanism produces low chi for beneficiaries and high chi for victims, confirming Tangled Rope structure. The perspectival gap is driven by how each agent's exit options feed into d: trapped/identity-locked → high d → high f(d) → high chi; arbitrage → low d → negative f(d) → negative chi (experienced as coordination benefit).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids false natural law classification by recognizing that algorithmic opacity is an architectural choice, not a law of nature. Platforms could operate with high transparency (as demonstrated by smaller or experimental platforms and regulatory compliance initiatives). The extraction component derives from platform market power and network effects, not from inherent technical necessity. The coordination component (preventing gaming, ensuring content quality at scale) is real but does not require full opacity — partial transparency is compatible with most coordination goals. The Tangled Rope classification reflects both components correctly: beneficiaries maintain opacity through enforced secrecy (active enforcement yes), some genuine coordination benefits flow to users and creators through algorithmic filtering (coordination function genuine), and asymmetric extraction occurs (victims identified: creators, users, information quality). The theater component (platforms claiming transparency while maintaining opacity) suggests the constraint is beginning to degrade (theater ratio increasing) — this may accelerate the regulatory coalition's sunset timeline. The classification is stable at Tangled Rope: not Rope because extraction is high and enforced, not Snare because genuine coordination benefits exist.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'What level of algorithmic transparency can be maintained while preventing adversarial gaming and manipulation without destroying the coordination function?',
    'Empirical testing of transparency interventions: partial disclosure trials (EU regulators, platform transparency initiatives), open-source recommendation systems, and comparative performance metrics between opaque and semi-transparent algorithms',
    'If high transparency is possible (>80%): current opacity is primarily extractive; classify as Snare from user/creator perspectives. If transparency requires operational compromise (<40%): opacity is primarily coordination; classify as Rope from platform perspective. If threshold is intermediate (40-80%): current opacity exceeds necessity (Tangled Rope confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Threshold of algorithmic transparency compatible with platform function').

omega_variable(
    alternative_platform_viability,
    'Do decentralized or federated platforms (Mastodon, Bluesky, open protocols) represent genuine structural alternatives to monopolistic platforms, or do they face inherent trade-offs (scalability, user experience, content quality) that reintroduce opacity as necessary?',
    'Comparative analysis of transparency levels across alternative platforms; user adoption barriers; scalability constraints; content moderation effectiveness; long-term sustainability of open-source alternatives',
    'If alternatives are truly viable: scaffold sunset is credible (regulatory pathways plus technical alternatives enable exit). If alternatives face inherent constraints: trap persists because even ''alternatives'' require opacity; victims are constrained rather than completely trapped but exit remains costly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms provide genuine escape from opacity constraints').

omega_variable(
    user_comprehension_asymmetry,
    'Do most platform users understand that algorithmic curation is occurring and that their information diet is filtered? Or is the opacity sufficiently deep that users experience the platform as ''natural'' reality?',
    'User studies on algorithmic literacy; survey data on user awareness of recommendation systems; behavioral analysis of how users respond when shown their algorithmic filter (e.g., through transparency features)',
    'If users are unaware (>70% don''t understand curation): suppression is high (users cannot perceive the constraint); classification as Snare is correct. If users are aware: suppression is lower (users understand the mechanism) but may still be trapped by exit costs; suppress might moderate toward Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_comprehension_asymmetry, empirical, 'User awareness of algorithmic curation mechanisms').

omega_variable(
    platform_black_box_authenticity,
    'Do platform operators genuinely not understand their own algorithms (machine-learning systems so complex they cannot predict outputs), or is this claimed opacity a strategic fiction?',
    'Analysis of platform technical documentation, academic literature on algorithmic interpretability, evidence of internal model auditing capabilities, comparison between platforms that claim opacity vs those that implement interpretability tools',
    'If genuine black box: the opacity cannot be lifted without platform rebuild; extraction is structurally high because unavoidable. If strategic fiction: opacity could be eliminated through architectural choices; extraction is intentional and higher than necessary. Either way, confirms Tangled Rope (both coordination and extraction occur), but directs intervention strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_black_box_authenticity, empirical, 'Whether algorithmic opacity is genuine (unavoidable) or strategic (chosen)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_curation_opacity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algcur_tr_t0, algorithmic_curation_opacity, theater_ratio, 0, 0.4).
narrative_ontology:measurement(algcur_tr_t5, algorithmic_curation_opacity, theater_ratio, 5, 0.55).
narrative_ontology:measurement(algcur_tr_t10, algorithmic_curation_opacity, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(algcur_be_t0, algorithmic_curation_opacity, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(algcur_be_t5, algorithmic_curation_opacity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(algcur_be_t10, algorithmic_curation_opacity, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_curation_opacity, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_curation_opacity, advertising_opacity).
narrative_ontology:affects_constraint(algorithmic_curation_opacity, social_media_engagement_maximization).
narrative_ontology:affects_constraint(algorithmic_curation_opacity, algorithmic_bias_amplification).
narrative_ontology:affects_constraint(algorithmic_curation_opacity, information_quality_degradation).

% DUAL FORMULATION NOTE:
% Algorithmic curation opacity is upstream of several secondary constraints: it enables predatory advertising (advertising_opacity), drives engagement-maximization that amplifies misinformation (engagement_maximization), concentrates platform power enabling bias amplification (algorithmic_bias), and degrades information quality through inverted incentives (information_degradation). The network shows dependency: if opacity were reduced, all downstream constraints would require restructuring.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_curation_opacity, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
