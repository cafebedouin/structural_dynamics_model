% ============================================================================
% CONSTRAINT STORY: platform_content_moderation_snare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_content_moderation_snare, []).

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
 *   constraint_id: platform_content_moderation_snare
 *   human_readable: Platform Content Moderation Snare
 *   domain: digital_platforms/content_moderation
 *
 * SUMMARY:
 *   Platform content moderation creates a structural snare: users depend
 *   entirely on centralized platforms for audience reach and discourse
 *   participation, platforms wield unilateral suspension authority with
 *   minimal recourse, and the mechanisms of suppression (algorithmic opacity,
 *   asymmetric appeals, network lock-in) prevent organized exit or
 *   negotiation. The constraint exhibits asymmetric extraction masked by
 *   coordination rhetoric: platforms claim moderation creates safe
 *   communities (a genuine coordination function), but the infrastructure
 *   simultaneously enables maximized extraction through visibility control,
 *   data harvesting, and content monetization. Content creators, marginalized
 *   communities, and the public discourse commons bear the costs of both
 *   moderation errors and algorithmic amplification bias; the platform
 *   captures the benefits. The theater ratio (0.58) reflects the substantial
 *   gap between stated moderation policies and actual implementation,
 *   amplified by compliance theater (transparency reports, oversight boards)
 *   that create appearance of accountability without commensurate actual
 *   constraint. Extractiveness has risen from 0.48 to 0.68 over the interval
 *   as platforms have strengthened enforcement mechanisms, increased
 *   algorithmic filtering opacity, and expanded suspension authority without
 *   proportional appeals infrastructure.
 *
 * KEY AGENTS:
 *   - Content Creators: Primary victims (powerless/trapped) — suspended without recourse; audience and income platform-locked; cannot negotiate or exit at scale
 *   - Marginalized Communities: Primary victims (powerless/trapped) — over-moderated by keyword filtering and cultural misunderstanding; suppressed through both removal and algorithmic demotion; no collective voice
 *   - Public Discourse Commons: Structural victim (powerless/trapped) — abstract collective good that cannot organize; bears cost of polarization and misinformation amplification; no mechanism for exit or voice
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — controls visibility, monetizes content, eliminates liability, shapes discourse; experiences moderation as coordination that enables their business model
 *   - Independent Creators and Moderators: Secondary actors (moderate/constrained) — benefit from platform distribution but face extraction through algorithmic control and policy unpredictability
 *   - Civil Rights and Regulatory Coalitions: Organized pressure (organized/constrained) — benefit from harm-reduction policies but suffer from extraction via suppression of advocacy and lack of transparency
 *   - Compliance Apparatus: Institutional theater (institutional/constrained) — performs accountability while actual enforcement remains opaque; sees own mechanisms as degraded
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_content_moderation_snare, 0.68).
domain_priors:suppression_score(platform_content_moderation_snare, 0.72).
domain_priors:theater_ratio(platform_content_moderation_snare, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_content_moderation_snare, extractiveness, 0.68).
narrative_ontology:constraint_metric(platform_content_moderation_snare, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(platform_content_moderation_snare, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_content_moderation_snare, snare).
narrative_ontology:human_readable(platform_content_moderation_snare, "Platform Content Moderation Snare").
narrative_ontology:topic_domain(platform_content_moderation_snare, "digital_platforms/content_moderation").

domain_priors:requires_active_enforcement(platform_content_moderation_snare).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_content_moderation_snare, platform_operators).
narrative_ontology:constraint_victim(platform_content_moderation_snare, content_creators).
narrative_ontology:constraint_victim(platform_content_moderation_snare, marginalized_communities).
narrative_ontology:constraint_victim(platform_content_moderation_snare, public_discourse_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUSPENDED CONTENT CREATOR (SNARE) — Cannot appeal removal decisions or migrate audience. Faces permanent account deletion without recourse. No alternative platform has equivalent reach. Trapped by network effects; suppression is total because the creator's audience, reputation, and livelihood are platform-locked. Maximum experienced extraction.
constraint_indexing:constraint_classification(platform_content_moderation_snare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MARGINALIZED COMMUNITIES (SNARE) — Over-moderated due to keyword filtering and proxy enforcement. Their speech norms and cultural references are misclassified as violations. No mechanism to contest mass removals or bias in moderation systems. Cannot migrate to alternative platforms at scale. Trapped by both network effects and algorithmic opacity. Suppression through both de facto removal and epistemic exclusion.
constraint_indexing:constraint_classification(platform_content_moderation_snare, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT CONTENT MODERATORS (TANGLED ROPE) — Benefit from platform commission structures and audience monetization, but face inconsistent content policies, rapid policy changes, and algorithmic demotion. Can migrate to competing platforms at moderate cost. Experience both coordination (platform provides distribution) and extraction (platform controls visibility and revenue terms). Suppression is significant but not total — some agency exists.
constraint_indexing:constraint_classification(platform_content_moderation_snare, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Benefits from moderation infrastructure: establishes community standards, controls discourse shape, eliminates regulatory liability, and monetizes content filtering decisions. Experiences moderation as coordination: communicating standards enables user autonomy within boundaries. Net beneficiary. High arbitrage options — can adjust policies, outsource moderation, or deploy AI systems. Extraction flows toward this agent.
constraint_indexing:constraint_classification(platform_content_moderation_snare, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVIL RIGHTS AND PUBLIC DISCOURSE COALITION (TANGLED ROPE) — Organized pressure groups benefit from platform content policies that reduce harassment and disinformation, but suffer extraction through algorithmic suppression of advocacy content and lack of transparency. Have some agency through regulatory pressure and public campaigns. Cannot fully exit because they need platform reach to advocate. Mixed experience: genuine coordination function (safer communities) alongside asymmetric extraction (visibility control).
constraint_indexing:constraint_classification(platform_content_moderation_snare, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PUBLIC DISCOURSE COMMONS (SNARE) — Abstract collective good that cannot organize or exit. Faces extraction through algorithmic amplification of engagement-maximizing content, suppression of consensus-building discourse, and platform control of the information environment. Bears the cost of polarization, viral misinformation, and erosion of shared epistemic foundations. No mechanism for collective voice or exit. Suppression is total because the commons has no institutional representation.
constraint_indexing:constraint_classification(platform_content_moderation_snare, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY AND COMPLIANCE APPARATUS (PITON) — Content moderation policies are partially performative: platforms deploy compliance theater (transparency reports, oversight boards, community guidelines) while maintaining algorithmic opacity and user-unfavorable policies. The regulatory apparatus sees its own process as degraded — enforcement mechanisms exist but are unevenly applied. Theater ratio high because stated policies diverge from actual enforcement. Piton classification reflects institutional inertia masked by cosmetic governance.
constraint_indexing:constraint_classification(platform_content_moderation_snare, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, content moderation constraints appear as immutable features of any mass communication system: some filtering is inherent to managing information flow at scale, conflict between speech freedom and harm prevention is unavoidable, and algorithmic bias is inherent to automation. This perspective risks naturalizing contingent institutional choices (profit-maximizing engagement algorithms, opacity by design, asymmetric appeal mechanisms) as laws of nature. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(platform_content_moderation_snare, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_content_moderation_snare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_content_moderation_snare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_content_moderation_snare, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_content_moderation_snare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_content_moderation_snare, TR),
    TR >= 0.70.

:- end_tests(platform_content_moderation_snare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. Platforms extract through multiple mechanisms: (1) visibility control via algorithmic ranking, which creators cannot predict or contest; (2) data harvesting from user engagement; (3) asymmetric monetization of creator content; (4) suspension authority with minimal appeals infrastructure. The rising trajectory reflects platform strengthening of enforcement mechanisms (increasing from 0.48 to 0.68 over 8 years) without proportional appeals infrastructure expansion. Suppression (0.72): High. Victims face multiple binding constraints: network effects (no alternative platform has equivalent reach), algorithmic opacity (cannot understand or predict removal decisions), asymmetric appeals (platform judges own cases), and account termination finality (no recourse). For marginalized communities, suppression is compounded by algorithmic bias and cultural misunderstanding in moderation. Theater ratio (0.58): Moderate-high. Transparency reports are published but show minimal actual policy change; content appeals boards exist but demonstrate low constraint on platform decisions; community guidelines diverge significantly from actual enforcement. Theater has risen as platforms have added governance structures while maintaining operational opacity. The gap between stated protections and actual user experience reveals the performative nature of compliance.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural data produces radically different classifications depending on the observer's position. The platform operator sees rope: moderation creates communities, enables safe discourse, and coordinates user behavior within beneficial boundaries. Organized civil society sees tangled_rope: genuine safety benefits exist alongside asymmetric extraction via suppression and opacity. Independent creators see tangled_rope: distribution benefits coexist with extraction via algorithmic control. Marginalized communities and suspended creators see snare: maximum extraction with no benefits and no exit. The public discourse commons sees snare: bears costs of misinformation and polarization amplification with no mechanism for voice or exit. The regulatory apparatus sees piton: moderation policies are maintained through inertia and theater rather than actual function. The civilizational observer risks seeing mountain: content moderation appears as an inevitable feature of mass communication systems. The analytical observer's mountain is a false summit — it naturalizes contingent choices (profit-maximizing algorithms, opacity by design, asymmetric appeal mechanisms) as laws of nature rather than recognizing them as deliberate architectural choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural relationships: beneficiaries with arbitrage options (platform operators) experience low or negative effective extraction; trapped victims with no exit (suspended creators, marginalized communities, public commons) experience maximum extraction; constrained agents with some options (organized civil rights groups, independent creators) experience moderate extraction. The platform operator's d-value is near zero (full beneficiary with escape options) — they can adjust policies, deploy alternative moderation systems, or moderate less stringently; extraction flows toward them. Suspended creators and marginalized communities face d-values near 1.0 (full target, no exit) — their only option is to accept suspension or abandon the platform entirely, losing audience and income. The organized civil rights coalition has d-value around 0.55 (constrained but organized) — they have some agency through regulatory pressure and public campaigns but cannot fully exit because they need platform reach. The public discourse commons has d-value near 1.0 (full target, no exit, no collective voice) — an abstract collective good with no mechanism to negotiate or escape.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare classification resolves the mandatrophy by disambiguating coordination function from extraction mechanism. Platform moderation genuinely coordinates some user behaviors: it reduces harassment, removes certain categories of abuse, and maintains community standards. These are real coordination benefits. Simultaneously, the same infrastructure enables asymmetric extraction: visibility control, data harvesting, and suspension authority with minimal recourse. The mandatrophy arises from conflating these two functions. The resolution: they are both real. The snare classification confirms that the coordination benefits (which might otherwise justify calling it rope or tangled_rope) are outweighed by the asymmetric extraction for the victims (suspended creators, marginalized communities, public commons). The beneficiary's experience (rope or pure benefit) and the victim's experience (snare) are both accurate from their structural positions. The constraint is a snare because the asymmetric extraction mechanism is a *constitutive feature* of the system design, not an incidental side effect — platforms deliberately maintain opacity, design asymmetric appeals, and leverage network effects to maximize extraction while providing coordination benefits as justification. Without the extraction, the coordination function could be achieved through transparent, symmetric, decentralized mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moderation_scale_tradeoff,
    'Is the scale of wrongful suspensions (error rate) fundamentally inevitable due to volume, or is it a result of under-investment in appeal mechanisms and human review?',
    'Comparative analysis of platforms with different appeal infrastructure investment; correlation between moderation error rates and resources allocated to human review and appeals',
    'If inevitable: extraction is incidental to coordination function. If discretionary: platforms are choosing extraction via under-investment in fairness mechanisms. Classification implications: if discretionary under-investment, the snare classification strengthens; if truly inevitable, some snare aspects reclass as mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(moderation_scale_tradeoff, empirical, 'Whether moderation errors are inevitable at scale or discretionary under-investment').

omega_variable(
    algorithmic_transparency_ceiling,
    'Can algorithmic content moderation ever be transparent enough to enable meaningful appeals, or is opacity intrinsic to the technical architecture?',
    'Technical feasibility analysis of interpretable moderation systems; empirical testing of whether creators can predict or understand removal decisions across platforms with varying transparency policies',
    'If transparency is technically feasible but withheld: suppression is deliberate (snare confirmed). If transparency is technically impossible: some suppression is inherent to the coordination function (snare downgrade possible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_transparency_ceiling, empirical, 'Whether algorithmic moderation can achieve meaningful transparency').

omega_variable(
    alternative_platform_viability,
    'Are network effects sufficient to permanently trap creators, or are alternative platforms (decentralized, federated, or specialized) capturing escape velocity?',
    'Longitudinal tracking of creator migration; comparison of user acquisition rates and retention for alternative platforms; analysis of whether critical mass thresholds are achievable for alternatives',
    'If alternatives are viable: exit_options upgrade from trapped to constrained for some creators, potentially downgrading snare to tangled_rope. If network effects are permanent: trap is structural, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether alternative platforms can break network effect lock-in').

omega_variable(
    marginalized_community_suppression_mechanism,
    'Are marginalized communities disproportionately suspended due to algorithmic bias, cultural misunderstanding by moderators, or deliberate policy targeting?',
    'Audit studies comparing suspension rates by demographic/linguistic markers; analysis of policy application to similar content from different user communities; qualitative research on moderation guideline interpretation',
    'If algorithmic bias: technical fix possible (though not implemented). If moderation mismatch: training required. If deliberate targeting: policy change required. Each mechanism suggests different remediation and reveals different aspects of the extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(marginalized_community_suppression_mechanism, empirical, 'Root cause of disparate suspension rates for marginalized communities').

omega_variable(
    governance_theater_substitution,
    'Do platforms'' transparency reports, content appeals boards, and oversight mechanisms meaningfully constrain moderation decisions, or are they substitutes for actual accountability?',
    'Analysis of platform response rates to oversight board recommendations; empirical correlation between published policies and actual enforcement; tracking of policy changes driven by external governance vs internal optimization',
    'If genuine constraint: governance structures are real coordination (snare downgrade possible). If theater: governance legitimates extraction without constraining it (snare confirmation, theater_ratio upgrade).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(governance_theater_substitution, empirical, 'Whether governance structures meaningfully constrain moderation extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_content_moderation_snare, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pcm_tr_t0, platform_content_moderation_snare, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pcm_tr_t4, platform_content_moderation_snare, theater_ratio, 4, 0.5).
narrative_ontology:measurement(pcm_tr_t8, platform_content_moderation_snare, theater_ratio, 8, 0.58).
narrative_ontology:measurement(pcm_tr_t2, platform_content_moderation_snare, theater_ratio, 2, 0.46).

% Extraction over time
narrative_ontology:measurement(pcm_be_t0, platform_content_moderation_snare, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(pcm_be_t4, platform_content_moderation_snare, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(pcm_be_t8, platform_content_moderation_snare, base_extractiveness, 8, 0.68).
narrative_ontology:measurement(pcm_be_t2, platform_content_moderation_snare, base_extractiveness, 2, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_content_moderation_snare, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(platform_content_moderation_snare, 0.12).
narrative_ontology:affects_constraint(platform_content_moderation_snare, algorithmic_amplification_bias).
narrative_ontology:affects_constraint(platform_content_moderation_snare, platform_data_extraction).
narrative_ontology:affects_constraint(platform_content_moderation_snare, creator_economic_dependency).

% DUAL FORMULATION NOTE:
% Content moderation is downstream of platform design choices (algorithmic amplification, data extraction, creator monetization). This story focuses on the moderation constraint itself as a snare. The upstream constraints have their own extractiveness values reflecting algorithmic bias (moderate-high), data extraction (very high), and economic lock-in (high). The moderation snare is structurally interdependent with these upstream constraints — the snare is enabled by opacity and lock-in built into the platform architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_content_moderation_snare, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
