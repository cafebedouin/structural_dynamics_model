% ============================================================================
% CONSTRAINT STORY: platform_algorithmic_curation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_algorithmic_curation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: platform_algorithmic_curation
 *   human_readable: Platform Algorithmic Curation
 *   domain: digital_economy/information_architecture
 *
 * SUMMARY:
 *   Platform algorithmic curation represents a global-scale coordination
 *   mechanism that has become the primary architecture for information
 *   distribution, attention allocation, and social connection. The constraint
 *   exhibits the full tangled rope signature: genuine coordination function
 *   (solving information overload, enabling creator reach, matching
 *   advertisers to audiences) coupled with asymmetric extraction (attention
 *   capture, behavioral modification, epistemic authority concentration, user
 *   and creator dependency). Extractiveness has risen over 15 years from 0.32
 *   to 0.58 as platforms have optimized engagement at the expense of
 *   epistemic quality and user autonomy. Theater ratio (0.68) reflects that
 *   much platform governance—content moderation standards, algorithmic
 *   transparency, diversity commitments, ethical review boards—operates as
 *   public narrative while actual optimization remains engagement-driven. The
 *   constraint meets all tangled rope gates: requires active enforcement
 *   (algorithms actively rank and suppress content), has genuine
 *   beneficiaries (platform operators, advertisers, engagement-optimized
 *   creators), has genuine victims (ordinary users trapped in feeds,
 *   misaligned creators, news ecosystems degraded by engagement
 *   optimization), and exhibits asymmetric extraction (extraction flows
 *   upward toward platforms and advertisers, costs distributed to users and
 *   epistemic commons).
 *
 * KEY AGENTS:
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture network effects, advertiser revenue, and behavioral data; maximum exit optionality and organizing power
 *   - Ordinary Users: Primary victims (powerless/trapped) — structurally dependent on platforms for social connection and information; no meaningful exit available at civilizational scale
 *   - Misaligned Creators: Secondary victims (powerless/identity_locked) — identity-fused with platform presence; identity capture operates alongside economic dependency
 *   - Advertisers: Secondary beneficiaries (powerful/constrained) — benefit from targeting precision but face platform dependency and monopoly pricing
 *   - Engagement-Optimized Creators: Beneficiary subclass (powerful/arbitrage or powerful/mobile) — align with algorithm through skill or accident; capture disproportionate reach and monetization
 *   - News Ecosystems and Epistemic Commons: Distributed victims (organized/constrained) — depend on platform reach but bear costs of degraded information quality and viral misinformation
 *   - Regulatory Authorities: Theater actors (organized/constrained) — appear to regulate algorithmic harms through compliance frameworks and transparency mandates, but enforcement is weak and compliance is performative
 *   - Analytical Observer: Civilization-scale view (analytical/analytical) — sees the constraint as a genuine hybrid with real coordination necessity and real extraction asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_algorithmic_curation, 0.58).
domain_priors:suppression_score(platform_algorithmic_curation, 0.65).
domain_priors:theater_ratio(platform_algorithmic_curation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_algorithmic_curation, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_algorithmic_curation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_algorithmic_curation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_algorithmic_curation, tangled_rope).
narrative_ontology:human_readable(platform_algorithmic_curation, "Platform Algorithmic Curation").
narrative_ontology:topic_domain(platform_algorithmic_curation, "digital_economy/information_architecture").

domain_priors:requires_active_enforcement(platform_algorithmic_curation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_algorithmic_curation, platform_operators).
narrative_ontology:constraint_beneficiary(platform_algorithmic_curation, advertisers).
narrative_ontology:constraint_beneficiary(platform_algorithmic_curation, engagement_optimized_creators).
narrative_ontology:constraint_victim(platform_algorithmic_curation, ordinary_users).
narrative_ontology:constraint_victim(platform_algorithmic_curation, creators_misaligned_with_algorithm).
narrative_ontology:constraint_victim(platform_algorithmic_curation, epistemic_commons).
narrative_ontology:constraint_victim(platform_algorithmic_curation, attention_scarcity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORDINARY USER (SNARE) — Trapped within the feed architecture; cannot exit without abandoning social connection, news access, and identity presence. Suppression is total: algorithmic curation appears natural, inevitable, unchallengeable. The user bears the cost of attention extraction and behavioral modification with minimal coordination benefit. Maximum experienced extraction.
constraint_indexing:constraint_classification(platform_algorithmic_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MISALIGNED CREATOR (SNARE) — Structurally mobile (can abandon platform, create offline) but identity-locked to the platform ecosystem. Professional identity fused with platform presence; perceived career requires algorithmic visibility. Exit would mean psychological death of the persona they constructed. The algorithm extracts labor and attention; suppression operates through identity capture rather than material barriers.
constraint_indexing:constraint_classification(platform_algorithmic_curation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Genuinely coordinates user attention, creator incentives, and advertiser reach. The algorithm solves the technical problem of information overload at scale. Experiences the constraint as pure coordination with no perceived extraction cost. Net beneficiary with maximum exit optionality. Coordination function genuine; but asymmetric extraction hidden beneath the coordination narrative.
constraint_indexing:constraint_classification(platform_algorithmic_curation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER (TANGLED ROPE) — Constrained by platform dependency but benefits from algorithmic targeting precision. Genuine coordination function (matching ads to receptive audiences) alongside extraction (monopoly pricing, data leverage). Can exit to other platforms but faces switching costs and reduced reach. Mixed extraction and coordination from the advertiser's perspective.
constraint_indexing:constraint_classification(platform_algorithmic_curation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (PITON) — Content moderation and algorithmic transparency regulations are largely performative theater. Platforms declare algorithmic values (diversity, accuracy, fairness) while optimizing for engagement. Regulatory pressure persists through institutional expectation (legislators must be seen regulating tech) but enforcement is weak and can be gamed. High theater ratio: compliance reports, explainability frameworks, ethics committees — all lacking enforcement teeth.
constraint_indexing:constraint_classification(platform_algorithmic_curation, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EPISTEMIC COMMONS / NEWS ECOSYSTEM (TANGLED ROPE) — Algorithmic curation genuinely coordinates information distribution at unprecedented scale but simultaneously extracts from epistemic reliability. Engagement-optimized algorithms suppress epistemic value in favor of emotional resonance. The news ecosystem depends on platform reach for distribution but bears the cost of degraded information quality and viral misinformation. Moderate power with constrained exit: news organizations can develop independent platforms but lose reach advantage.
constraint_indexing:constraint_classification(platform_algorithmic_curation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, algorithmic curation is a genuine hybrid: it solves the coordination problem of attention allocation at scale (essential function) while simultaneously extracting attention, behavioral modification, and epistemic authority from users and creators. The constraint meets all gates for tangled rope: beneficiaries (platform, advertisers), victims (users, creators, epistemic commons), active enforcement (algorithmic ranking), asymmetric extraction (extraction flow concentrates upward). This is the primary classification.
constraint_indexing:constraint_classification(platform_algorithmic_curation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_algorithmic_curation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_algorithmic_curation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_algorithmic_curation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_algorithmic_curation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_algorithmic_curation, TR),
    TR >= 0.70.

:- end_tests(platform_algorithmic_curation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, reflecting continuous optimization of engagement metrics at cost to user autonomy and epistemic quality. The measurement trajectory (0.32 → 0.45 → 0.58) shows accumulation as platforms have increased algorithmic optimization depth—from basic relevance ranking to affective targeting to behavioral prediction. Plateau at 0.58-0.62 suggests maturation of current engagement-optimization paradigm rather than further increase. Suppression (0.65): Moderate-high. Users cannot exit without material social and informational cost; creators cannot escape algorithmic dependency without career disruption; regulators cannot enforce accountability against platform opacity. But suppression is not total—users and creators understand the mechanism (even if they cannot escape it); alternative platforms exist (at reduced scale); regulatory frameworks create legal constraints on the most egregious harms. Theater ratio (0.68): High. Content moderation standards (community guidelines, harmful-content removal, fact-checking) are genuinely enforced but operate at small scale relative to total content. Transparency initiatives (explainability reports, algorithmic documentation) describe surface features while optimization targets (engagement, watch-time, click conversion) remain opaque. Diversity and fairness commitments exist as policy but are not primary optimization targets. This high theater reflects the gap between stated algorithmic values and actual optimization incentives.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across the six types: Snare (powerless users, identity-locked creators), Rope (platform operators), Tangled Rope (advertisers, news ecosystems, analytical observer), and Piton (regulatory theater). No perspective sees the same constraint type as any other major perspective. This gap is diagnostic: it reveals that the constraint's legitimacy is entirely position-dependent. The platform operator's rope is the ordinary user's snare. The regulator's theater is the user's inescapable reality. The analytical observer must navigate this gap by recognizing all perspectives as structurally valid—not as error or delusion, but as accurate reports from different positions in the extraction hierarchy.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint operates through multiple mechanisms that differentiate directionality across agents. Platform operators benefit from network effects (more users increase value for all users) while extracting through behavioral data monopoly and attention capture. Advertisers benefit from precision targeting while bearing extraction costs through platform monopoly pricing and switching costs. Ordinary users bear the full extraction cost (attention, behavioral modification, epistemic degradation) while gaining coordination benefit (connection, information access) that they could obtain through alternative mechanisms at lower extraction cost. Misaligned creators face highest suppression because their identity is constituted through the platform—they cannot exercise their theoretical mobile exit option because exit means psychological death of their professional persona. This differentiation drives the directionality derivation: beneficiaries have low d (0.05–0.20); constrained beneficiary-victims have moderate d (0.55); trapped victims have high d (0.85–0.95); identity-locked victims have very high d (0.89) despite theoretical mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED. The constraint classification as tangled rope is robust against mischaracterization as pure rope (coordination only) or pure snare (extraction only). The mandatrophy arises from whether algorithmic curation is a necessary coordination mechanism or an optional rentier extraction. Four interpretations exist: (1) Rope + Snare Decomposition: Algorithmic curation solves a genuine coordination problem (attention allocation at scale) AND enables extractive monopolization of the coordination solution. Decompose into two constraints: algorithmic_coordination_mechanism (ε=0.15, Rope) and algorithmic_attention_monopoly (ε=0.72, Snare). (2) Tangled Rope (Current): Algorithmic curation is a single constraint that simultaneously coordinates and extracts; the coordination function cannot be separated from the extraction mechanism without losing efficiency. (3) Pure Snare (Critical View): Algorithmic engagement optimization is parasitic on coordination needs; alternative non-extractive coordination architectures (federated protocols, user-controlled ranking) could solve the coordination problem without extraction. The algorithm is a solution to a vendor-created problem. (4) Necessary Extraction (Platform View): Algorithmic optimization is genuinely necessary for coordination at the scale required by modern information environments; the extraction premium is the coordination cost at scale. Each interpretation requires different empirical evidence (omega variables identify which measurements resolve the mandatrophy). Current evidence suggests interpretation (2)—tangled rope—is most accurate, but the classification cannot be certified without resolving the empirical questions in the omegas.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_necessity_threshold,
    'Below what platform size does algorithmic curation stop being functionally necessary and become purely extractive?',
    'Comparative analysis of information quality metrics (relevance, accuracy, epistemic diversity) on: large algorithmic platforms vs small community platforms vs non-algorithmic social networks; measurement of signal-to-noise ratio and user-reported satisfaction',
    'If low threshold: algorithm is optional rental extraction, reclassifies toward pure snare. If high threshold: algorithm is genuine coordination requirement, reclassifies toward rope. Classification magnitude changes significantly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_necessity_threshold, empirical, 'Platform size threshold at which algorithmic curation stops being functionally necessary').

omega_variable(
    behavioral_modification_intent,
    'To what degree does engagement optimization constitute intentional behavioral modification (extraction mechanism) versus incidental user preference matching?',
    'Analysis of algorithmic design documentation, internal platform optimization targets, A/B testing protocols, and decision trees; comparison between stated user preferences and algorithmic recommendations to identify preference-distortion patterns',
    'If intentional: suppression is deliberate, extracted value includes attention and behavioral modification, classification maintains high ε. If incidental: suppression is lower, classification may shift toward rope. Impacts whether the constraint is Tangled Rope or Rope + Snare decomposition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_modification_intent, empirical, 'Degree of intentional behavioral modification in algorithmic optimization').

omega_variable(
    creator_capture_mechanism,
    'Is identity_locked appropriate for misaligned creators, or is exit_options actually ''constrained'' with high material cost?',
    'Longitudinal tracking of creator platform abandonment: measurement of career disruption upon platform exit; analysis of alternative income viability; interviews on perceived identity fusion vs economic dependency',
    'If identity_locked: binding mechanism is psychological, constraint is more severe for the identity-fused agent. If constrained: binding is primarily economic, exit is theoretically possible at cost. Changes the snare classification''s immutability signature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_capture_mechanism, empirical, 'Whether creator entrapment is identity-based or economically-based').

omega_variable(
    algorithmic_transparency_sufficiency,
    'Can meaningful user agency be restored through transparency (explainability + control), or is the extraction mechanism fundamentally uncontrollable at scale?',
    'Comparative study of platforms with transparency/control features vs opaque platforms: measurement of user attention patterns, creator behavior change, advertiser reach efficiency, and engagement metrics after transparency intervention',
    'If transparency sufficient: constraint could shift to scaffold with sunset (transparency-driven exit path). If insufficient: transparency is pure theater (piton component), constraint remains tangled rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether transparency and control can mitigate algorithmic extraction').

omega_variable(
    alternative_coordination_mechanisms,
    'Do viable non-extractive alternatives for attention coordination exist at global scale, or is centralized algorithmic curation a necessary coordination floor?',
    'Analysis of federated protocol platforms (ActivityPub), decentralized social networks, and user-controlled filtering systems; measurement of coordination effectiveness, information quality, and network effects compared to centralized algorithms',
    'If alternatives viable: current algorithms are rentier extraction, strong mandate for decomposition/replacement. If alternatives fail: algorithms may be coordination floor with unavoidable efficiency premium. Affects whether classification should be snare or tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Viability of non-algorithmic coordination at global scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_algorithmic_curation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_alg_tr_t0, platform_algorithmic_curation, theater_ratio, 0, 0.45).
narrative_ontology:measurement(plat_alg_tr_t5, platform_algorithmic_curation, theater_ratio, 5, 0.58).
narrative_ontology:measurement(plat_alg_tr_t10, platform_algorithmic_curation, theater_ratio, 10, 0.68).
narrative_ontology:measurement(plat_alg_theater_plateau, platform_algorithmic_curation, theater_ratio, 15, 0.71).

% Extraction over time
narrative_ontology:measurement(plat_alg_be_t0, platform_algorithmic_curation, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(plat_alg_be_t5, platform_algorithmic_curation, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(plat_alg_be_t10, platform_algorithmic_curation, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(plat_alg_extract_plateau, platform_algorithmic_curation, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_algorithmic_curation, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_algorithmic_curation, 0.18).
narrative_ontology:affects_constraint(platform_algorithmic_curation, attention_scarcity).
narrative_ontology:affects_constraint(platform_algorithmic_curation, creator_dependency_ecosystem).
narrative_ontology:affects_constraint(platform_algorithmic_curation, epistemic_quality_degradation).
narrative_ontology:affects_constraint(platform_algorithmic_curation, behavioral_data_monopoly).

% DUAL FORMULATION NOTE:
% Platform algorithmic curation is upstream of multiple dependent constraints in the digital economy. The coordination component (solving information overload) affects attention scarcity and creator reach. The extraction component (engagement optimization, behavioral modification) drives behavioral data monopoly accumulation and epistemic quality degradation. These downstream constraints are structurally dependent on algorithmic curation and share its ε value as a baseline; their own ε values measure additional extraction layers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_algorithmic_curation, powerful, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
