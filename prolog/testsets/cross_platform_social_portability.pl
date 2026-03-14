% ============================================================================
% CONSTRAINT STORY: cross_platform_social_portability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_platform_social_portability, []).

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
 *   constraint_id: cross_platform_social_portability
 *   human_readable: Cross-Platform Social Portability Lock-In
 *   domain: digital_infrastructure/social_media
 *
 * SUMMARY:
 *   Cross-platform social portability represents a structural constraint
 *   where users' social capital — their followers, conversation history,
 *   connections, and accumulated identity across years of platform use —
 *   becomes economically locked to a single platform provider. The constraint
 *   exhibits genuine coordination function (the platform aggregates billions
 *   of users into connected graphs) alongside asymmetric extraction (users
 *   cannot move without abandoning their social capital). Extractiveness has
 *   risen from 0.35 to 0.58 over the measurement interval as platforms have
 *   progressively monetized user graph data, implemented algorithmic opacity,
 *   and closed APIs. Theater has increased as well as regulatory compliance
 *   creates performative data export features that provide minimal
 *   portability benefit. The constraint's mandatrophy manifests in the false
 *   summit risk: network effects are often presented as immutable laws of
 *   physics ('larger networks always win'), but comparative analysis of
 *   email, SMS, and emerging federated protocols (ActivityPub, Bluesky AT)
 *   reveals that many technical barriers to portability are policy choices
 *   rather than physical impossibilities. The EU Digital Markets Act and GDPR
 *   data portability rights represent a scaffold perspective — regulatory
 *   mandates introducing sunset logic where forced interoperability and API
 *   access create alternative exit paths.
 *
 * KEY AGENTS:
 *   - User Base: Primary victim (powerless/trapped) — cannot exit without losing accumulated social capital; structurally immobilized by network effects
 *   - Content Creators: Secondary victim (moderate/constrained) — benefit from platform distribution but bear extraction through opaque algorithms, revenue share asymmetry, and content moderation power; face high cost to migrate audience
 *   - Incumbent Platforms (Meta, X, TikTok, etc.): Primary beneficiary (institutional/arbitrage) — extract monopoly rent from user immobility while providing genuine aggregation and coordination service
 *   - Alternative Platform Providers: Structural competitor (moderate/constrained) — locked out from accessing incumbent graphs; cannot differentiate on portability; high infrastructure costs for features incumbents provide at scale
 *   - Digital Rights Coalition (EU DMA, civil society, regulators): Organized intervener (organized/constrained) — implementing regulatory portability mandates creating policy-driven sunset logic; constrained by incumbent political influence
 *   - Technology Community (protocol developers, open standards orgs): Infrastructure layer (organized/constrained) — building federated alternatives (ActivityPub, AT Protocol) but face adoption barriers from incumbent network effects
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent network effects as immutable physics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_platform_social_portability, 0.58).
domain_priors:suppression_score(cross_platform_social_portability, 0.68).
domain_priors:theater_ratio(cross_platform_social_portability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_platform_social_portability, extractiveness, 0.58).
narrative_ontology:constraint_metric(cross_platform_social_portability, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(cross_platform_social_portability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_platform_social_portability, tangled_rope).
narrative_ontology:human_readable(cross_platform_social_portability, "Cross-Platform Social Portability Lock-In").
narrative_ontology:topic_domain(cross_platform_social_portability, "digital_infrastructure/social_media").

domain_priors:requires_active_enforcement(cross_platform_social_portability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_platform_social_portability, incumbent_platforms).
narrative_ontology:constraint_victim(cross_platform_social_portability, user_base).
narrative_ontology:constraint_victim(cross_platform_social_portability, alternative_platforms).
narrative_ontology:constraint_victim(cross_platform_social_portability, social_capital_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NETWORK-TRAPPED USER (SNARE) — User cannot port follower relationships, conversation history, or social graph to alternative platforms. Exit imposes total loss of accumulated social capital. Powerless to change platform economics. Bears full extraction cost with no coordination benefit — the platform's network effects are a pure constraint on this agent's mobility.
constraint_indexing:constraint_classification(cross_platform_social_portability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CREATOR WITH PLATFORM DEPENDENCE (TANGLED ROPE) — Content creators benefit from distribution, analytics, and monetization infrastructure while simultaneously bearing extraction through algorithmic opacity, revenue share asymmetry, and content control. High cost to exit (audience loss, income disruption) but not total (can migrate followers gradually through multi-platform presence). Genuine coordination (the platform aggregates distribution) alongside asymmetric extraction.
constraint_indexing:constraint_classification(cross_platform_social_portability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INCUMBENT PLATFORM (ROPE) — Experiences network lock-in as a coordination mechanism: the platform solves the genuine problem of aggregating attention and enabling connection. The platform benefits from user immobility but genuinely provides coordination value. Net beneficiary with arbitrage options — can migrate to adjacent markets, capture new user segments, or leverage existing graph.
constraint_indexing:constraint_classification(cross_platform_social_portability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE PLATFORM PROVIDER (TANGLED ROPE) — Structurally locked out: cannot access the incumbent's social graph to offer portability as a competitive advantage. Must rebuild user bases from zero. Simultaneously benefits from interoperability standards and open protocols where they exist (ActivityPub, open graph formats), but coordination benefit is weak compared to extraction burden. High fixed costs for infrastructure they cannot differentiate with.
constraint_indexing:constraint_classification(cross_platform_social_portability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: DIGITAL RIGHTS COALITION (SCAFFOLD) — Regulatory and advocacy organizations (EU Digital Markets Act, data portability directives, interoperability requirements) are implementing sunset logic: forced data portability, API access, and graph export create alternative paths for mobility. Low effective extraction because organized agents see real agency and a policy-driven exit path. Theater is moderate — compliance appears performative (dark exports, minimal usability) until interoperability standards mature.
constraint_indexing:constraint_classification(cross_platform_social_portability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: NETWORK EFFECT NARRATIVE (PITON) — The 'network effects make portability impossible' framing has become institutional inertia. Technical constraints (API compatibility, social graph ownership, identity federation) are presented as natural laws, but many are policy choices. Theater ratio is moderate because genuine technical coordination barriers exist alongside contingent architectural decisions. The narrative persists despite degrading technical basis — containerized microservices and federated protocols prove partial portability is technically feasible, but the story of inevitability persists through institutional inertia.
constraint_indexing:constraint_classification(cross_platform_social_portability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational scale, network effects create immutable lock-in: larger networks always have economic advantage, users rationally cluster on dominant platforms, and this creates an inevitable winner-take-most dynamic. This perspective risks naturalizing what is actually a contingent architectural and policy choice. The engine's false summit detector will identify this as misclassification — the 'immutable' network effect rests on technical decisions (proprietary APIs, opaque graph ownership, identity siloing) that are policy-reversible.
constraint_indexing:constraint_classification(cross_platform_social_portability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_platform_social_portability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_platform_social_portability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_platform_social_portability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_platform_social_portability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_platform_social_portability, TR),
    TR >= 0.70.

:- end_tests(cross_platform_social_portability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Users lose access to their social graph upon exit — an irreversible cost equivalent to capital extraction. The 0.58 value reflects that platforms do provide coordination benefits (aggregation, connection infrastructure, discovery mechanisms) alongside the lock-in extraction. Pure extraction would exceed 0.70; this hybrid allows for meaningful coordination. The rising trajectory (0.35→0.58) reflects increasing monetization of graph data and algorithmic amplification control. Suppression (0.68): High. Barriers to exit include: (1) informational — users often unaware of portability options or data export usability; (2) technical — API incompatibility, identity federation fragmentation; (3) economic — creator income dependent on platform; (4) social — friends remain on incumbent platform. Suppression is not total because some users do switch and regulatory mandates create limited export/access options. Theater ratio (0.55): Moderate. Genuine coordination function (aggregation of billions into connected graph) is substantial and real. But theater has increased as platforms introduce GDPR exports and data APIs that comply with regulations while providing minimal portability usability — users can export data but cannot easily re-import to alternatives. Theater reflects the gap between compliance appearance and actual switching friction reduction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximum perspectival divergence across all six types. No dominant classification emerges because the structure genuinely supports multiple valid readings. The key tension: Is network lock-in a coordination mechanism (Rope) or an extraction mechanism (Snare)? The answer is: both, from different structural positions. For the platform, it coordinates supply and demand. For the user, it extracts immobility cost. For regulators, it is a temporary coordination failure with a policy sunset (Scaffold). For the platform's narrative about inevitability, it naturalizes as Mountain — but the false summit detector catches this as policy misrepresented as physics. The perspectival gap is not a measurement error; it is the constraint's defining feature.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (incumbent platform): institutional power, arbitrage exit, benefits from network size. Derives d=0.05 (full beneficiary), f(d) ≈ -0.12 (experiences negative extraction — the constraint subsidizes them). Victims (user base, alternative platforms): powerless/moderate power, trapped/constrained exit, bear costs of immobility. Derive d=0.85-0.95 (full target), f(d) ≈ 1.15-1.42 (high experienced extraction). The directionality spread is structural — it reflects genuine asymmetry in who bears costs and who captures benefits. Regulatory organizations occupy middle ground: organized power, constrained exit (limited political leverage against incumbent lobbying), but clear beneficiary (digital rights, user agency). Derive d=0.40 (partial victim position), f(d) ≈ 0.40 (lower extracted extraction due to organizational capacity and policy agency). No overrides needed — the automatic derivation captures the structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSIS: The constraint's mandatrophy is resolved through perspectival pluralism, not type convergence. The false summit risk is the 'network effects are physics' narrative. From the analytical observer's civilizational position, the constraint appears as Mountain: larger networks always economically dominate, winner-take-most dynamics are inevitable, users rationally cluster on dominant platforms. This framing naturalizes contingent architectural and policy choices (proprietary APIs, opaque graph ownership, identity siloing) as immutable laws. The mandatrophy resolver: comparative analysis across communication technologies shows that portability IS technically feasible. Email is portable (open protocols, federated delivery, decentralized identity). SMS is portable (intercarrier agreements, number portability regulations). ActivityPub-based social networks demonstrate federated coordination at meaningful scale. The constraint is not Mountain (immutable network physics) but Tangled Rope (coordination + policy-enforced extraction). The false summit dissolves when observing from the regulatory/technical community perspective: portability is blocked by policy choices, not physics. This resolves the mandatrophy by showing that type divergence reflects genuine structural differences in how agents experience the constraint, not measurement ambiguity or analyst error.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_feasibility_portability,
    'What fraction of social graph portability constraints are genuinely technical vs. policy/architectural choices?',
    'Comparative analysis of portability across platforms with different API openness levels; assessment of interoperability success in email, messaging (Signal/Matrix), and federated social (Mastodon/ActivityPub) ecosystems',
    'If mostly technical: constraint is closer to Mountain (immutable network physics). If mostly policy: constraint is Snare/Tangled Rope (policy-reversible extraction). Current evidence suggests 30-40% technical, 60-70% policy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_feasibility_portability, empirical, 'Ratio of technical to policy-driven portability barriers').

omega_variable(
    user_switching_cost_distribution,
    'How are switching costs distributed: primarily loss of social capital (network effect) or loss of content, identity, interaction history, and monetization?',
    'User surveys on switching barriers; comparative cost analysis across platforms; measurement of re-activation friction after portability options are available',
    'If primarily social capital: network effects are the binding mechanism (constraint is immutable given network dynamics). If distributed across multiple costs: portability of one dimension (graph, identity, content) reduces total switching cost significantly (constraint becomes negotiable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_switching_cost_distribution, empirical, 'Distribution of user switching costs across network effect, content, identity, and monetization dimensions').

omega_variable(
    regulatory_portability_effectiveness,
    'Do mandated data export and API access requirements (EU GDPR, DMA) actually reduce switching costs or merely provide performative compliance?',
    'Longitudinal tracking of platform switching rates post-GDPR and post-DMA implementation; measurement of export usability (automated re-import vs manual migration) and adoption rates',
    'If effective: scaffold sunset is real and constraint degradation is underway. If performative: regulation creates compliance theater without reducing extraction; constraint remains Snare for powerless agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_portability_effectiveness, empirical, 'Whether regulatory portability mandates reduce actual switching friction').

omega_variable(
    identity_locked_user_commitment,
    'For users trapped on platforms, is immobility driven by high structural switching costs (trapped) or by identity fusion with the platform/social graph (identity_locked)?',
    'Qualitative analysis of user narratives about platform attachment; measurement of switching intention vs actual switching when barriers are lowered; assessment of identity-reconstructing rhetoric (''I am my Twitter presence'')',
    'If mostly trapped: removing technical barriers substantially increases mobility. If mostly identity_locked: even with portability, users remain bound by cognitive frame; psychological or community-based interventions needed alongside technical portability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_user_commitment, empirical, 'Ratio of structural switching costs to identity fusion in user platform lock-in').

omega_variable(
    alternative_coordination_feasibility,
    'Can federated, open-protocol social networks (ActivityPub, nostr, Bluesky''s AT Protocol) actually coordinate at scale comparable to incumbent platforms, or do they face inherent coordination costs?',
    'Comparative analysis of federation latency, consistency, moderation scalability, and discovery mechanisms; measurement of engagement and retention on federated platforms vs incumbents at equivalent user cohort sizes',
    'If feasible: alternative platforms offer genuine coordination (constraint becomes pure extraction via market power, not technical inevitability). If unfeasible: network topology itself creates unavoidable coordination costs that justify incumbent scale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Whether federated social protocols can achieve coordination efficiency comparable to centralized platforms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_platform_social_portability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(csport_tr_t0, cross_platform_social_portability, theater_ratio, 0, 0.4).
narrative_ontology:measurement(csport_tr_t5, cross_platform_social_portability, theater_ratio, 5, 0.48).
narrative_ontology:measurement(csport_tr_t10, cross_platform_social_portability, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(csport_be_t0, cross_platform_social_portability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(csport_be_t5, cross_platform_social_portability, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(csport_be_t10, cross_platform_social_portability, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_platform_social_portability, resource_allocation).
narrative_ontology:affects_constraint(cross_platform_social_portability, attention_monopoly_platform_markets).
narrative_ontology:affects_constraint(cross_platform_social_portability, data_portability_rights).
narrative_ontology:affects_constraint(cross_platform_social_portability, federated_identity_protocols).

% DUAL FORMULATION NOTE:
% Cross-platform social portability decomposes into three distinct constraints: (1) attention_monopoly_platform_markets (ε≈0.65, Snare) — the incumbent's monopoly extraction from network effects; (2) data_portability_rights (ε≈0.30, Scaffold) — regulatory interventions with sunset logic; (3) federated_identity_protocols (ε≈0.25, Rope) — alternative coordination mechanisms. The combined story captures the systemic interaction; decomposed stories model specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
