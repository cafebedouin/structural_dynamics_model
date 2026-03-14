% ============================================================================
% CONSTRAINT STORY: platform_gatekeeping_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_gatekeeping_consolidation, []).

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
 *   constraint_id: platform_gatekeeping_consolidation
 *   human_readable: Platform Gatekeeping Consolidation
 *   domain: digital_economy/technology/governance
 *
 * SUMMARY:
 *   Platform gatekeeping consolidation describes the structural extraction
 *   mechanism by which dominant digital platforms (social media, app stores,
 *   payment processors, cloud infrastructure) control access to users and
 *   markets, capturing asymmetric revenue and enforcing unilateral terms on
 *   dependent service providers, creators, and competitors. Over the past
 *   decade (2014–2024), as platforms achieved network effect dominance, the
 *   gatekeeping constraint has intensified: extraction has grown from 0.35 to
 *   0.58, theater has risen from 0.42 to 0.58 (driven by compliance theater
 *   around content moderation, data privacy regulations, and antitrust
 *   investigations), and suppression has remained high (0.62) through
 *   deplatforming risk, algorithmic demotion, and API deprecation. The
 *   constraint exhibits all eight DR types, making it a diagnostic exemplar
 *   for institutional extraction at scale. From the emerging platform's
 *   perspective, it is a snare (no exit without recreating the network from
 *   zero). From the dominant platform's perspective, it is rope (coordination
 *   problem solved). From the regulatory coalition's perspective, it is
 *   tangled rope (genuine need for platform governance + asymmetric
 *   rule-setting power). From the interoperability standard movement's
 *   perspective, it is a scaffold with a credible sunset (10–20 years for
 *   sufficient adoption to shift network effects toward federation rather
 *   than concentration). From the legacy telecom regulation's perspective, it
 *   is a piton (common carrier rules persist in law but are unenforced
 *   against tech platforms). The constraint's theater ratio reflects
 *   performative compliance: platforms commit to transparency, moderation
 *   appeals, and data portability while simultaneously embedding extraction
 *   into algorithmic feeds, recommendation opacity, and API throttling.
 *
 * KEY AGENTS:
 *   - Dominant Platforms (Meta, Google, Apple, Amazon): Institutional/arbitrage beneficiaries — control access to users; capture revenue and data; set rules unilaterally; face minimal exit risk
 *   - Emerging Platforms and Startups: Powerless/trapped victims — require platform access to reach users; cannot negotiate terms; face existential risk of deplatforming; suppression is high through algorithm demotion and API deprecation
 *   - Independent Creators: Powerless/trapped victims — depend on platform algorithms for visibility; face arbitrary rule changes, shadow-banning, demonetization; no alternative distribution mechanisms
 *   - Mid-Tier Service Providers: Moderate/constrained — Uber, DoorDash, Spotify type — can negotiate but face revenue capture (30% cuts, unfavorable algorithm ranking); constrained rather than trapped because of partial market power
 *   - Large Alternative Platforms: Powerful/mobile — TikTok, Discord, Telegram gaining foothold but still constrained by network effects and data disadvantage relative to incumbents
 *   - Regulatory Coalitions: Organized/constrained — EU (DSA/DMA), FTC, national regulators attempt to enforce interoperability and data portability but face lobbying resistance and implementation lag
 *   - Interoperability Standard Movement: Organized/constrained — ActivityPub, Matrix, ATProtocol developers building federated alternatives; growing adoption but critical mass still 5-10 years away
 *   - Legacy Telecom Regulators: Institutional/arbitrage — Common carrier frameworks and net neutrality rules theoretically applicable but persistently unenforced; theater-driven rather than functionally constraining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_gatekeeping_consolidation, 0.58).
domain_priors:suppression_score(platform_gatekeeping_consolidation, 0.62).
domain_priors:theater_ratio(platform_gatekeeping_consolidation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_gatekeeping_consolidation, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_gatekeeping_consolidation, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(platform_gatekeeping_consolidation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_gatekeeping_consolidation, tangled_rope).
narrative_ontology:human_readable(platform_gatekeeping_consolidation, "Platform Gatekeeping Consolidation").
narrative_ontology:topic_domain(platform_gatekeeping_consolidation, "digital_economy/technology/governance").

domain_priors:requires_active_enforcement(platform_gatekeeping_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_gatekeeping_consolidation, dominant_platforms).
narrative_ontology:constraint_beneficiary(platform_gatekeeping_consolidation, incumbent_service_providers).
narrative_ontology:constraint_victim(platform_gatekeeping_consolidation, emerging_platforms).
narrative_ontology:constraint_victim(platform_gatekeeping_consolidation, independent_creators).
narrative_ontology:constraint_victim(platform_gatekeeping_consolidation, alternative_services).
narrative_ontology:constraint_victim(platform_gatekeeping_consolidation, end_users_limited_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Perspective 1: EMERGING PLATFORM / INDEPENDENT CREATOR (SNARE) — Structurally trapped. To reach users, must comply with dominant platform rules (algorithm feeds, content moderation, payment structures) with no genuine alternatives. Suppression is high: deplatforming is existential; algorithm demotion is slow starvation; restrictive API terms block integration. The trap is complete because the dominant platform controls access to the userbase. Exit would require recreating the entire network from zero — economically infeasible.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Perspective 2: MID-TIER SERVICE PROVIDER (TANGLED ROPE) — Constrained but not trapped. Can negotiate with platforms, build multi-platform presence, or develop off-platform features — but at significant cost. Experiences genuine coordination benefit (access to platform users enables their service) alongside asymmetric extraction (platforms take revenue share, set rules unilaterally, threaten deplatforming). Mixed: the platform solves a real distribution problem, but also exploits the provider's dependence.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Perspective 3: DOMINANT PLATFORM (ROPE) — Net beneficiary with arbitrage options. Experiences the gatekeeping constraint as coordination: managing content, matching supply to demand, scaling infrastructure. Benefits from network effects, data capture, and revenue extraction. Can arbitrage between competing ecosystems (migrate users, acquire competitors, expand into adjacent markets). The constraint solves a real coordination problem: without gatekeeping, the platform becomes a chaotic commons.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4: REGULATORY COALITION / CIVIL SOCIETY (TANGLED ROPE) — Organized but constrained. Can lobby for interoperability standards, data portability rules, and content governance transparency — but platforms lobby harder. EU Digital Markets Act, DMA, and similar frameworks represent attempts to enforce alternative coordination mechanisms. Genuine coordination problem exists (platforms do need to moderate, manage networks), but extraction manifests as regulatory capture and lobbying asymmetry. The coalition is building enforcement mechanisms (DSA, DMA) but faces generational lag before implementation maturity.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 5: LEGACY TELECOM REGULATION (PITON) — Common carrier frameworks, net neutrality, and interconnection requirements are theoretically applicable but persistently unenforced against tech platforms. These regulatory instruments persist through institutional inertia and legislative lag, not because they effectively constrain platforms. The mechanism is degraded: platforms circumvent through framing (we are publishers, not carriers; we are private companies exercising editorial discretion). Theater ratio is high because the debate centers on whether legacy rules apply, not on enforcement. Piton classification reflects the persistence of rules that no longer function.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 6: LARGE ALTERNATIVE PLATFORM (TANGLED ROPE) — Mobile enough to exit (can build independent user base, seek venture capital, or leverage different market segment) but constrained by network effects and data disadvantage. Experiences gatekeeping as both coordination problem (users expect interoperability, data sync) and extraction mechanism (dominant platform's data, user trust, and algorithmic advantage create structural barriers to competition). Mixed: genuine need for standards + asymmetric competitive moat.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Perspective 7: INTEROPERABILITY STANDARD MOVEMENT (SCAFFOLD) — Open protocols (ActivityPub, Matrix, OpenID) represent alternative coordination mechanisms with built-in sunset: as adoption grows, the extraction mechanism (centralized gatekeeping) loses force because data portability and protocol-level interoperability enable users and creators to defect. Suppression is declining as standards mature and regulatory pressure increases (DMA portability requirements, DSA obligations). Theater is moderate — standards are technically complex but functionally real. Sunset rationale: 10-20 years for sufficient adoption to shift network effects away from single dominant platforms.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Perspective 8: ANALYTICAL OBSERVER / NETWORK EFFECTS NATURALIZATION (MOUNTAIN) — From a civilizational/universal perspective, network effects create a structural inevitability: platforms that achieve critical mass attract both users and providers, creating winner-take-most dynamics that appear immutable. This perspective risks naturalizing what is actually a contingent institutional arrangement (lack of interoperability standards, data portability, and regulatory frameworks that could enable multi-platform ecosystems). The engine's false summit detector will identify this as naturalization of a policy choice, not a law of nature.
constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_gatekeeping_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_gatekeeping_consolidation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_gatekeeping_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_gatekeeping_consolidation, TR),
    TR >= 0.70.

:- end_tests(platform_gatekeeping_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. Dominant platforms capture 30% of transaction revenue (payment processing, app store), control algorithmic visibility (ranking creators, suppressing competitors), and extract data on all user activity. The measurement shows growth from 0.35 (2014, when platforms were smaller and less coordinated) to 0.58 (2024, as consolidation completed and API restrictions tightened). The value reflects measured asymmetry: dominant platforms set terms unilaterally; dependent actors cannot negotiate. Suppression (0.62): High. Barriers to exit include network effects (users on the dominant platform drive creators there; creators on the dominant platform drive user adoption), switching costs (data portability is incomplete; contact lists are siloed), deplatforming risk (arbitrary enforcement of ambiguous terms), and API deprecation (platforms kill integrations to reduce competitive threats). Emerging platforms cannot build competing services without platform access. Theater ratio (0.58): Moderate-high and rising. Compliance theater includes: commitment to content moderation transparency (with opaque appeals processes), data privacy certifications (followed by algorithmic data extraction), antitrust compliance statements (coupled with non-interoperable API design), and diversity pledges (while suppressing marginalized creators who violate unstated rules). The measurement shows growth from 0.42 to 0.58 as regulatory pressure increased (DSA, DMA, FTC investigations, state-level legislation) and platforms invested in compliance appearance.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's true nature. Emerging platforms see an immutable barrier (snare); dominant platforms see a coordination mechanism (rope); regulators see a solvable governance problem (tangled rope → scaffold with sunset); interoperability advocates see technological alternatives emerging (scaffold); legacy regulators see rules that should constrain platforms but don't (piton); and civilizational analysts risk misidentifying contingent extraction as inevitable network effects (false summit mountain). The gap indicates that the constraint's classification depends entirely on the observer's structural position and exit options. No single type is 'correct' — the presheaf over all perspectives reveals the full extraction structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) reflects each agent's structural position relative to extraction flow. Dominant platforms (institutional/arbitrage) have low d (~0.10–0.20): they benefit, have exit options (acquire competitors, pivot to adjacent markets), and experience the constraint as coordination. Emerging platforms (powerless/trapped) have high d (~0.95): they depend entirely on platform access, cannot exit, and bear maximum extraction. Mid-tier providers (moderate/constrained) have moderate-high d (~0.70): they can negotiate but face structural disadvantage. Regulatory coalitions (organized/constrained) have d ~0.55: they have agency and enforcement mechanisms but face lobbying resistance and institutional lag. Large alternatives (powerful/mobile) have d ~0.50: competitive but constrained by network effects. The engine derives d from beneficiary/victim declarations + exit options and computes chi (effective extractiveness) using the sigmoid f(d) and scope modifier σ(S=global=1.2). Global scope amplifies chi because suppression and extraction mechanisms operate everywhere simultaneously — deplatforming in one jurisdiction affects all users and creators globally.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves potential mandatrophy by mapping each classification type to a distinct structural reality. Snare is the emerging platform's genuine experience: no escape without recreating the network. Rope is the dominant platform's genuine experience: they solve a real coordination problem. Tangled rope is the mid-tier provider's genuine experience: mixed coordination (market access) and extraction (revenue capture). Scaffold is the interoperability standard movement's genuine experience: real alternative mechanisms with credible sunset as adoption grows. Piton is the legacy regulator's genuine experience: rules persist without functional enforcement. Mountain (false summit) is the naturalization trap: claiming platform dominance is inevitable because of network effects, when it is actually contingent on policy choices around interoperability. The mandatrophy resolves by showing that all six types are legitimate perspectival readings of the same structural data — the constraint is truly multi-typed depending on the observer's structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effects_inevitability,
    'Are platform monopolies an inevitable consequence of network effects (mountain) or a contingent outcome of interoperability policy choices (snare/tangled rope)?',
    'Historical analysis of platforms with strong interoperability (email, SMS, web hosting) vs walled gardens (social media, app stores); counterfactual policy modeling of mandatory data portability on incumbent dominance; international comparison of DMA-enforced interoperability adoption rates',
    'If inevitable: classification shifts toward mountain; regulation is futile. If contingent: classification remains snare/tangled rope; regulatory intervention and standard-setting are structurally meaningful.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effects_inevitability, preference, 'Whether platform dominance is inevitable from network effects or contingent on interoperability policy').

omega_variable(
    interoperability_cost_threshold,
    'At what cost of implementation do interoperability standards (data portability, API access, content migration) become so burdensome that they fail to reduce gatekeeping extraction?',
    'Comparison of DMA compliance costs (EU) vs. actual user switching rates post-implementation; analysis of which data portability formats actually enable competing platforms to onboard users; measurement of API throttling and deprecation as post-compliance extraction mechanisms',
    'If compliance costs are high and switching rates remain low: scaffolds fail to deliver sunset, classification remains tangled rope. If compliance enables switching: scaffold sunset is validated, theater ratio drops below 0.50.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_cost_threshold, empirical, 'Cost threshold at which interoperability standards become operationally infeasible').

omega_variable(
    regulatory_capture_feedback,
    'Does regulatory pressure (DSA, DMA, FTC enforcement) reduce gatekeeping extraction or does it generate regulatory arbitrage where platforms comply locally while intensifying extraction globally?',
    'Longitudinal tracking of platform behavior pre- vs post-regulation by jurisdiction; analysis of compliance theater (performative rule-following) vs functional behavior change; measurement of differential enforcement across markets',
    'If capture/arbitrage dominates: regulation becomes theater (piton classification), suppression remains high. If regulation enforces functional change: suppression declines, theater ratio drops, scaffold perspective is validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_feedback, empirical, 'Whether regulation reduces extraction or generates capture and arbitrage').

omega_variable(
    user_preferences_versus_switching_costs,
    'Do users stay on dominant platforms because of genuine preference for features/network or because switching costs (data loss, contact fragmentation, relearning) are prohibitively high?',
    'Survey of reasons for platform stickiness; analysis of user behavior when switching costs are removed (controlled experiments with data portability, bulk contact import, UI parity); international comparison of switching rates in jurisdictions with vs without data portability mandates',
    'If switching costs dominate: the constraint is suppression-driven (high lock-in); interoperability standards should reduce both suppression and extraction. If preferences dominate: network effects are real; interoperability standards may not reduce gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_preferences_versus_switching_costs, empirical, 'User platform loyalty driven by preference vs switching costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_gatekeeping_consolidation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pgc_tr_t0, platform_gatekeeping_consolidation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pgc_tr_t5, platform_gatekeeping_consolidation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(pgc_tr_t10, platform_gatekeeping_consolidation, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pgc_be_t0, platform_gatekeeping_consolidation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pgc_be_t5, platform_gatekeeping_consolidation, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(pgc_be_t10, platform_gatekeeping_consolidation, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_gatekeeping_consolidation, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_gatekeeping_consolidation, 0.18).
narrative_ontology:affects_constraint(platform_gatekeeping_consolidation, algorithmic_opacity_amplification).
narrative_ontology:affects_constraint(platform_gatekeeping_consolidation, data_portability_fragmentation).
narrative_ontology:affects_constraint(platform_gatekeeping_consolidation, venture_capital_dependency).
narrative_ontology:affects_constraint(platform_gatekeeping_consolidation, content_moderation_asymmetry).

% DUAL FORMULATION NOTE:
% Platform gatekeeping consolidation is upstream of multiple extractive sub-constraints. Algorithmic opacity enables gatekeeping by hiding ranking criteria. Data fragmentation prevents competitors from onboarding users. Venture dependency perpetuates gatekeeping by channeling capital through incumbent-aligned networks. Content moderation asymmetry enforces gatekeeping through selective enforcement. Each sub-constraint has its own ε value and operates through different mechanisms, but all are structurally dependent on the primary gatekeeping constraint. Decomposition into separate stories enables analysis of which mechanisms are most resistant to regulatory intervention.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_gatekeeping_consolidation, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
