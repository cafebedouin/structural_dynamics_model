% ============================================================================
% CONSTRAINT STORY: network_effect_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_network_effect_lock_in, []).

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
 *   constraint_id: network_effect_lock_in
 *   human_readable: Network Effect Lock-In
 *   domain: economics/technology/platform_dynamics
 *
 * SUMMARY:
 *   Network effect lock-in represents a constraint where the mechanism of
 *   coordination (connecting users at scale) becomes inseparable from the
 *   mechanism of extraction (capturing value from that coordination and
 *   preventing switching). The constraint exhibits tangled rope structure:
 *   genuine coordination benefits exist (users value being on a network with
 *   many participants) alongside asymmetric extraction (the platform operator
 *   captures disproportionate value and can restrict or degrade service). The
 *   suppression mechanism is dual: structural (exit requires achieving
 *   competing network elsewhere) and institutional (platform operators use
 *   contractual restrictions, API closures, and feature bundling to maintain
 *   lock-in). The theater ratio is moderate (0.48) because actual
 *   coordination happens, but significant theatrical compliance exists around
 *   false interoperability claims and staged regulatory compliance. The
 *   constraint's extractiveness has grown over the interval (0.35 → 0.58) as
 *   platforms have accumulated user data and expanded their extraction
 *   mechanisms beyond primary services. This growth mirrors the shift from
 *   genuine coordination problems (matching users on a network) to
 *   rent-seeking behaviors (data extraction, surveillance, forced
 *   participation in advertising and payment services).
 *
 * KEY AGENTS:
 *   - Locked-In Users: Primary victims (powerless/trapped) — have migrated social capital, contacts, and data to platform; exit costs are severe and perceived as insurmountable
 *   - Incumbent Platform Operator: Primary beneficiary (institutional/arbitrage) — captures value from network effects; has exit options to adjacent markets; benefits from same coordination mechanism that locks in users
 *   - Competing Platforms: Secondary victims (moderate/constrained) — face asymmetric coordination problem (critical mass paradox); cannot attract users without network, but network won't form without users; suppression is structural
 *   - Interoperability Coalition: Organized victims (organized/constrained) — coalition of users, competitors, regulators seeking to reduce lock-in through open standards and data portability; sees generational path to exit
 *   - Regulatory Intervention Coalition: Organized actors (organized/mobile) — antitrust authorities and data protection regulators; treating lock-in as temporary coordination failure with explicit sunset (EU Digital Markets Act, Data Portability Regulations)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes genuine coordination function underlying the extraction; sees fundamental tension between network value and exit option
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(network_effect_lock_in, 0.58).
domain_priors:suppression_score(network_effect_lock_in, 0.65).
domain_priors:theater_ratio(network_effect_lock_in, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(network_effect_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(network_effect_lock_in, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(network_effect_lock_in, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(network_effect_lock_in, tangled_rope).
narrative_ontology:human_readable(network_effect_lock_in, "Network Effect Lock-In").
narrative_ontology:topic_domain(network_effect_lock_in, "economics/technology/platform_dynamics").

domain_priors:requires_active_enforcement(network_effect_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(network_effect_lock_in, incumbent_platform_operator).
narrative_ontology:constraint_victim(network_effect_lock_in, locked_in_users).
narrative_ontology:constraint_victim(network_effect_lock_in, potential_competitors).
narrative_ontology:constraint_victim(network_effect_lock_in, interoperability_advocates).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN USER (SNARE) — User has migrated social capital, data, and contacts to the platform. Exit costs are severe: losing access to the network of other users, sacrificing accumulated data and history, abandoning network-dependent applications. The user perceives the constraint as unchangeable within biographical time horizon. Maximum extraction experienced.
constraint_indexing:constraint_classification(network_effect_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING PLATFORM (SNARE) — New entrants face an asymmetric coordination problem: they must achieve critical mass without the network that attracts users. Suppression is severe — users won't migrate without the alternative being established, but the alternative can't be established without users. High extraction via market foreclosure despite structural mobility (competitor could theoretically exit the market entirely).
constraint_indexing:constraint_classification(network_effect_lock_in, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: INTEROPERABILITY COALITION (TANGLED ROPE) — Coalition of users, competitors, and regulators sees genuine coordination function: the network itself provides value by connecting users. But this coordination is now extraction-heavy because the incumbent controls both the coordination mechanism and the extraction of value. Organized agents see exit path through regulatory intervention and open-standard infrastructure (generational timeline). Constrained in present, mobile in future.
constraint_indexing:constraint_classification(network_effect_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination mechanism: the network effect solves the problem of user acquisition. The operator benefits from the same network that locks in users. Arbitrage options exist: can exit to adjacent markets (cloud services, advertising, payment processing) while maintaining platform dominance. Effective extraction runs toward this agent.
constraint_indexing:constraint_classification(network_effect_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY INTERVENTION COALITION (SCAFFOLD) — Regulatory actors (antitrust authorities, interoperability mandates, data portability laws) see the lock-in as a temporary coordination failure with a sunset. Interventions like EU's Digital Markets Act and Data Portability Regulations are designed to reduce suppression and create exit pathways within a generational timeframe. Theater is moderate (compliance costs are real but enforcement mechanism is being built), and sunset is explicit.
constraint_indexing:constraint_classification(network_effect_lock_in, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational horizon, network effects are simultaneously a genuine coordination mechanism and an extraction device. The same property that makes the network valuable (everyone else is here) becomes the lock-in mechanism. The constraint cannot be dissolved without destroying the coordination benefit. Genuine ambiguity: is the extraction parasitic on coordination or intrinsic to it?
constraint_indexing:constraint_classification(network_effect_lock_in, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(network_effect_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(network_effect_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(network_effect_lock_in, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(network_effect_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(network_effect_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The platform operator captures increasing rents from user data, forces participation in bundled services, and prevents users from taking their social capital and data elsewhere. The value extracted has grown as platforms expanded from connectivity coordination into surveillance, advertising, and financial services. The measurement trajectory (0.35 → 0.58) reflects this accumulation. Not as high as pure snare (0.66+) because users do derive genuine value from being on a large network — the coordination benefit is real. Suppression (0.65): High and multifaceted. Structural suppression: users cannot easily rebuild their network elsewhere (critical mass problem). Institutional suppression: platform actively prevents interoperability (API restrictions, data lock-in, account linking requirements). Psychological suppression: users believe exit is impossible even when technically feasible (identity lock). Theater ratio (0.48): Moderate. Actual coordination happens — the network does connect users at scale. But significant theater exists: fake interoperability claims, staged regulatory compliance, elaborate terms-of-service theater that obscures actual extraction mechanisms. Theater is lower than in pure piton because the coordination function is not degraded — it remains genuinely functional, just extraction-heavy.
 *
 * PERSPECTIVAL GAP:
 *   Perspectival gap is maximal between the locked-in user (snare: trapped exit, powerless, biographical horizon) and the incumbent operator (rope: arbitrage exit, institutional, immediate horizon). The gap reveals that the constraint is simultaneously a coordination mechanism (rope from incumbent's view) and a lock-in device (snare from user's view) because they occupy opposite positions in the extraction flow. The scaffold perspective (regulatory intervention) claims to resolve the gap by introducing an explicit sunset, but this assumes regulators can successfully decouple the coordination benefit from the extraction mechanism — an assumption the analytical observer questions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent is derived from beneficiary/victim status and exit options. Locked-in users are victims with trapped exit (d ≈ 0.95 → high f(d) → high χ). Incumbent operator is beneficiary with arbitrage exit (d ≈ 0.05 → negative f(d) → negative χ, meaning low experienced extraction from this agent's perspective — they experience the constraint as enabling, not extractive). Competing platforms are victims with constrained exit (d ≈ 0.85 → high f(d)), but their effective χ is dampened by their organized power status (can lobby regulators, build coalitions). Interoperability coalition is organized victims with constrained exit but organized power (d ≈ 0.70 → f(d) moderate), giving them agency despite extraction. This produces the perspectival divergence: the same constraint has d ≈ 0.95 for powerless users and d ≈ 0.05 for the institutional operator, creating completely different experienced extractiveness values from the two perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that network effects create a structural condition where coordination and extraction become inseparable from the incumbent operator's perspective. The mandatrophy question: 'Is this coordination that extracts value, or extraction disguised as coordination?' cannot be answered abstractly — it depends on whether the extraction is parasitic on the coordination (removable via regulation) or intrinsic to it (removal would destroy the network benefit). The analytical observer's tangled_rope classification flags this ambiguity explicitly: the constraint contains both genuine coordination and genuine extraction, and their relative weights depend on counterfactual analysis (what would happen if users could exit costlessly?). The scaffold perspective's sunset clause presupposes that the two can be decoupled — that open-standard interoperability can preserve coordination while removing extraction. This is empirically testable: if email-style federated networks achieve feature parity with proprietary platforms, the decoupling succeeded. If not, the extraction is intrinsic. Current evidence is mixed: email remains decentralized but social networking has never successfully remained fully federated (Mastodon is decentralized but lacks Facebook's feature set; Signal remains closed-source despite openness claims). The mandatrophy resolution requires explicit tracking of whether regulatory interventions successfully separate coordination from extraction, or merely transfer the extraction mechanism to a new architectural form.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_extraction_boundary,
    'Is the extractive portion of network lock-in parasitic on a genuine coordination function, or is extraction intrinsic to how network effects operate?',
    'Counterfactual analysis: design an interoperable network architecture and measure whether users migrate if given costless exit. If they remain (preferring the incumbent''s feature set), extraction is lower than measured. If they leave immediately, extraction is intrinsic to the network effect mechanism itself.',
    'If parasitic: constraint can be decomposed into coordination (rope) plus extraction overlay. Regulatory intervention can separate the two. If intrinsic: extraction cannot be removed without destroying the coordination value — lock-in is a feature of network effects, not a bug. Constraint remains tangled_rope regardless.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether lock-in extraction is parasitic on coordination or intrinsic').

omega_variable(
    critical_mass_threshold_enforcement,
    'What minimum user base is required for a competing platform to achieve credible network parity, and is this threshold enforced by the incumbent or by the mathematics of network effects?',
    'Empirical: examine historical platform migrations (e.g., MySpace→Facebook, Twitter→Bluesky migration failure) and measure the incumbent''s defensive actions (pricing, feature bundling, exclusive agreements) vs. organic user retention. If incumbent''s defensive actions are necessary to prevent migration, suppression is institutional. If users remain even without defensive action, suppression is structural.',
    'If institutional suppression dominates: constraint is primarily tangled_rope (requires active enforcement). Regulatory intervention on that enforcement can reduce lock-in. If structural suppression dominates: network math creates lock-in without incumbent effort. Constraint approaches snare for locked-in users. Exit requires either: (a) superior features that overcome network disadvantage, or (b) coordinated mass migration (difficult to organize).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(critical_mass_threshold_enforcement, empirical, 'Source of suppression: incumbent actions or network mathematics').

omega_variable(
    interoperability_cost_viability,
    'Can interoperability standards (open APIs, data portability, protocol federation) achieve cost parity with proprietary networks, or do they require permanent subsidy or regulatory mandate?',
    'Cost accounting: compare infrastructure costs of operating a fully interoperable network (accounting for routing, synchronization, security overhead) against proprietary single-operator architecture. Historical comparison: email (federated, open) vs. WhatsApp (proprietary). If email-style architecture shows inherent cost disadvantage, interoperability sunset clause is at risk.',
    'If cost-viable: scaffold perspective is strong — regulatory mandates can transition to sustainable interoperable architecture. If cost-prohibitive: scaffold is aspirational theater — interoperability requires permanent subsidy or becomes unstable. Constraint remains tangled_rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_cost_viability, empirical, 'Long-term viability of interoperable architecture').

omega_variable(
    switching_cost_perception_gap,
    'To what extent is perceived switching cost higher than actual switching cost? Are users identity-locked to the platform (identity fusion) or materially trapped (data loss, contact loss)?',
    'Behavioral analysis: measure user perceptions of exit cost via survey, then compare to actual data recovery and contact-rebuilding costs in practice. Examine users who do migrate: did they recover contacts/history easily? Did identity expectations shift post-migration? Identity lock shows persistent framing of the platform as ''not replaceable'' even after successful migration. Material trap shows users cite data loss and contact loss as the barriers.',
    'If primarily identity-locked: constraint can be partially decomposed into a cognitive/identity story separate from the material network effect. Reframing campaigns might reduce experienced extraction. If primarily material trap: constraint is purely structural, resistant to cognitive intervention. Exit requires technical solutions (data portability, contact federation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_perception_gap, empirical, 'Switching cost: perception vs. material reality').

omega_variable(
    regulatory_intervention_spillover,
    'Does regulatory intervention to reduce lock-in in one jurisdiction (e.g., EU''s interoperability mandate) create competitive pressure that forces global compliance, or do incumbents fragment the product to maintain platform-specific lock-in in non-regulated markets?',
    'Longitudinal analysis: 3-5 years post-intervention, measure whether: (a) interoperability mandate is adopted globally by incumbent, (b) incumbent fragments product (EU vs. non-EU versions), (c) competitors successfully migrate users from EU to alternative platforms, (d) alternative platforms emerge that are interoperable from launch.',
    'If spillover occurs: regulatory intervention becomes effective globally — scaffold sunset is achievable. If fragmentation occurs: constraint remains tangled_rope — incumbents reduce extraction in regulated markets but maintain it elsewhere. Network effects are still leveraged for lock-in, just with regional differentiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_spillover, empirical, 'Whether regulatory intervention has global spillover effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(network_effect_lock_in, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(netlock_tr_t0, network_effect_lock_in, theater_ratio, 0, 0.32).
narrative_ontology:measurement(netlock_tr_t5, network_effect_lock_in, theater_ratio, 5, 0.4).
narrative_ontology:measurement(netlock_tr_t10, network_effect_lock_in, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(netlock_be_t0, network_effect_lock_in, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(netlock_be_t5, network_effect_lock_in, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(netlock_be_t10, network_effect_lock_in, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(network_effect_lock_in, resource_allocation).
narrative_ontology:affects_constraint(network_effect_lock_in, platform_switching_cost).
narrative_ontology:affects_constraint(network_effect_lock_in, data_portability_technical_constraint).

% DUAL FORMULATION NOTE:
% Network effect lock-in decomposes into two structurally distinct constraints: (1) the coordination mechanism (matching users at scale), which is genuinely valuable and has low extractiveness; and (2) the institutional lock-in (preventing users from leaving), which has high extractiveness. These should be written as separate stories (coordination_network_formation with ε≈0.15 as rope, and institutional_platform_lock_in with ε≈0.62 as snare). The present story treats them as inseparable because they operate on the same technical substrate, but their ε values differ by a factor of four, suggesting decomposition into a constraint family linked by network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(network_effect_lock_in, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
