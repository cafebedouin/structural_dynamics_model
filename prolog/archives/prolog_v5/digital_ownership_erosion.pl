% ============================================================================
% CONSTRAINT STORY: digital_ownership_erosion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_ownership_erosion, []).

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
 *   constraint_id: digital_ownership_erosion
 *   human_readable: Digital Ownership Erosion
 *   domain: technology/political_economy
 *
 * SUMMARY:
 *   Digital ownership erosion describes the structural shift from consumer
 *   ownership of digital artifacts (software, data, media) to leasehold
 *   models where users access functionality through restricted licenses and
 *   terms of service. The constraint operates through platform stickiness,
 *   API lock-in, proprietary data formats, legal barriers (DMCA, copyright
 *   enforcement), and network effects. This generates a matrix of structural
 *   relationships across six constraint types: end users face a snare
 *   (trapped in platform ecosystems with no exit); independent developers
 *   face snares (API deprecation destroys invested capital); content creators
 *   face tangled ropes (genuine coordination benefit mixed with asymmetric
 *   control); platform operators experience ropes (network effects as
 *   coordination); organized alternative builders face tangled ropes (genuine
 *   decentralization potential constrained by switching-cost coordination
 *   barriers); regulators maintain pitons (enforcement theater); analytical
 *   observers risk false summits (naturalizing network effects as immutable
 *   law). The constraint's theater ratio (0.58) reflects the gap between
 *   nominal user agency (privacy controls, data portability rights, account
 *   settings) and functional agency (these controls rarely enable meaningful
 *   exit).
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — cannot extract data, social graphs, or digital artifacts; all alternatives require coordinated migration
 *   - Independent Developers: Secondary victims (powerless/trapped) — depend on platform APIs; API changes destroy application value with no compensation mechanism
 *   - Content Creators: Mixed victims/beneficiaries (moderate/constrained) — benefit from distribution platform but lack ownership of audience data and algorithmic reach
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture all network value; switching costs and API lock-in are coordinated benefits to them
 *   - Open Source Coalition: Organized actors (organized/constrained) — building decentralized alternatives but face coordination problem (critical mass) that is itself the constraint to solve
 *   - Regulatory Bodies: Institutional actors (institutional/arbitrage) — implement data protection and interoperability mandates; enforcement is theatrical and insufficient to restore functional agency
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing network effects and treating lock-in as inevitable rather than policy-contingent
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_ownership_erosion, 0.58).
domain_priors:suppression_score(digital_ownership_erosion, 0.62).
domain_priors:theater_ratio(digital_ownership_erosion, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_ownership_erosion, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_ownership_erosion, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(digital_ownership_erosion, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_ownership_erosion, tangled_rope).
narrative_ontology:human_readable(digital_ownership_erosion, "Digital Ownership Erosion").
narrative_ontology:topic_domain(digital_ownership_erosion, "technology/political_economy").

domain_priors:requires_active_enforcement(digital_ownership_erosion).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_ownership_erosion, platform_operators).
narrative_ontology:constraint_beneficiary(digital_ownership_erosion, content_distribution_networks).
narrative_ontology:constraint_beneficiary(digital_ownership_erosion, subscription_service_providers).
narrative_ontology:constraint_victim(digital_ownership_erosion, end_users).
narrative_ontology:constraint_victim(digital_ownership_erosion, independent_developers).
narrative_ontology:constraint_victim(digital_ownership_erosion, cultural_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped. Once integrated into platform ecosystems, users cannot extract their data, social graphs, or digital artifacts without catastrophic loss. Technical barriers (API deprecation, proprietary formats) and legal barriers (terms of service, DMCA enforcement) combine to make exit impossible. No meaningful alternatives exist at scale. Maximum extraction: users bear labor costs (data generation, attention), platform captures value.
constraint_indexing:constraint_classification(digital_ownership_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT DEVELOPER (SNARE) — Trapped by dependency on platform APIs. Initial access enables ecosystem participation; subsequent API changes, rate limiting, or deprecation (announced with minimal notice) destroy application functionality. Third-party developers have no contractual protections, no representation in platform governance, and no exit except abandonment of investment. Platform captures all rents once ecosystem dependency is established.
constraint_indexing:constraint_classification(digital_ownership_erosion, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: CONTENT CREATOR (TANGLED ROPE) — Constrained by network effects. Platform provides distribution, discovery, and monetization mechanisms that have genuine coordination value. But creators have no ownership rights to their content repository, audience data, or algorithmic reach. Platform can demonetize, suppress distribution, or delete accounts unilaterally. Extraction is real (asymmetric control of audience and revenue) but mixed with genuine coordination benefit (access to distribution infrastructure creators could not build independently).
constraint_indexing:constraint_classification(digital_ownership_erosion, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences the constraint as coordination. Network effects and switching costs are genuine coordination mechanisms — they solve the problem of maintaining critical mass for platform utility. Lock-in is a feature, not a bug, from the beneficiary's perspective. The platform captures value asymmetrically, but this is framed as compensation for infrastructure provision and risk capital. Exit via acquisition or IPO provides exit options.
constraint_indexing:constraint_classification(digital_ownership_erosion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SOURCE COALITION (TANGLED ROPE) — Organized but structurally constrained. Building decentralized alternatives (federation protocols, blockchain-based ownership, distributed social networks) provides coordination benefits but faces chicken-and-egg problems: users won't migrate without critical mass, but critical mass requires millions of users to coordinate a migration. Coordination function is real (these systems can reduce platform lock-in); extraction persists because the coordination pathway requires overcoming switching costs that only decentralized infrastructure can solve. Active enforcement required to overcome network effect stickiness.
constraint_indexing:constraint_classification(digital_ownership_erosion, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY REGIME (PITON) — Data protection regulations (GDPR, CCPA) and interoperability mandates claim to restore user agency and reduce platform lock-in. But enforcement is weak, compliance costs are bundled into TOS updates that users cannot negotiate, and the regulatory theater (data download portability buttons, privacy dashboards) provides perceived agency without functional ownership restoration. Theater ratio: high. The constraint persists because legal mechanisms cannot scale to the complexity of data extraction and reconstruction without coordination mechanisms that regulations alone cannot provide.
constraint_indexing:constraint_classification(digital_ownership_erosion, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects are a fundamental property of information systems. Value increases with scale, switching costs are mathematically inherent to multi-agent systems, and the dynamics of lock-in appear as immutable laws. This perspective risks naturalizing what is actually a contingent policy choice: the current legal regime (copyright, DMCA, terms of service enforceability) is not a law of physics but a set of institutional arrangements that could be restructured to preserve network benefits while enabling interoperability and data portability.
constraint_indexing:constraint_classification(digital_ownership_erosion, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_ownership_erosion_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_ownership_erosion, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_ownership_erosion, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_ownership_erosion, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_ownership_erosion, TR),
    TR >= 0.70.

:- end_tests(digital_ownership_erosion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platforms capture asymmetric value from user-generated data and attention while users cannot exit without coordinating alternatives. However, users do receive genuine service value (search, social connectivity, content discovery), preventing the constraint from reaching pure extraction (ε > 0.70). The measurement trajectory shows erosion: early platforms competed on openness (0.32 at t=0); as network effects accumulated, lock-in mechanisms increased (0.58 at t=20). Suppression (0.62): High. Multiple enforcement mechanisms operate: technical (API changes, proprietary formats), legal (DMCA, copyright), contractual (TOS with unilateral modification rights), and economic (switching costs amplified by network effects). But not absolute — some API documentation exists, data export is technically possible (though incomplete), and alternative platforms exist (though at scale disadvantage). Theater ratio (0.58): Moderate-high. Regulatory responses (GDPR data downloads, privacy dashboards, interoperability mandates) provide visible user-control mechanisms but fail to translate into functional ownership restoration because the coordination problem (where to migrate to) remains unsolved. User agency over data is nominally protected but practically constrained.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reflects different structural positions' incommensurable experiences. End users experience pure extraction (snare) — they are locked in with no functional exit. Platform operators experience coordination (rope) — network effects solve the problem of maintaining critical mass. The gap is not empirical disagreement but structural difference: lock-in functions as extraction for trapped agents and as genuine coordination benefit for beneficiaries. Content creators occupy the middle ground (tangled rope): they perceive both coordination value and extraction asymmetry simultaneously. The analytical observer risks collapsing the gap by treating network effects as natural laws (mountain) rather than policy-contingent institutional arrangements. Regulators maintain theatrical solutions (piton) that appear to address the problem without solving the coordination barrier. The decentralized alternative coalition perceives a different problem entirely: not lock-in but the coordination problem of critical-mass transition (tangled rope), solvable in principle through protocol standardization but constrained by switching-cost barriers that only a coordinated migration can overcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators benefit asymmetrically from lock-in; their d value is low (0.15-0.25, derived from beneficiary + arbitrage exit options). End users are pure targets; their d value is high (0.95, derived from victim + trapped exit). Content creators derive mixed value; their d is moderate (0.60-0.70, derived from mixed beneficiary/victim status + constrained exit). The analytical observer treats network effects as natural, producing d ≈ 0.73 from canonical analytical fallback. The regulatory regime has institutional power but limited functional enforcement capacity, producing d ≈ 0.50-0.55 from institutional + constrained exit.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: Digital ownership erosion resolves the mandatrophy by distinguishing genuine coordination (network effects, platform interoperability) from artificial lock-in (legal barriers, API deprecation cycles, data format proprietary constraints). The constraint contains both: platform operators genuinely solve network-assembly problems (coordination value), but they also erect barriers designed to prevent exit (extraction mechanism). The mandatrophy is resolved by decomposing the constraint into two components: (1) network_coordination_value (ε ≈ 0.15-0.25, Rope) — genuine value of platform infrastructure; (2) lock_in_extraction_mechanism (ε ≈ 0.35-0.50, Snare/Tangled Rope) — artificial barriers preventing exit and enabling rent extraction. Current policy treats them as unified, which prevents effective intervention. The piton classification of regulatory responses reflects that data portability mandates and interoperability requirements address lock-in mechanically without solving the coordination problem that makes lock-in sticky: users have no obvious place to migrate to. The scaffold perspective (Open Source Coalition) identifies this: decentralization is solvable in principle through protocol coordination, but requires overcoming switching-cost barriers that constitute a second-order coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_vs_artificial_lock_in,
    'How much of measured platform lock-in stems from genuine network effects versus artificial switching costs created by legal restrictions (DMCA, copyright enforcement, TOS)?',
    'Comparative case study: network effects in interoperable systems (email, web) versus walled gardens (proprietary mobile platforms). Analysis of switching costs in jurisdictions with strong interoperability mandates versus weak ones.',
    'If lock-in is primarily artificial: extractiveness drops significantly (ε → 0.35-0.42), classification shifts from Snare to Tangled Rope for powerless agents. If primarily network-effect-driven: classification holds, but enables targeted intervention (remove legal lock-in rather than attempt to break network effects).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_vs_artificial_lock_in, empirical, 'Proportion of lock-in from network effects versus legal restrictions').

omega_variable(
    data_portability_sufficiency,
    'Can technically enabling data export and transfer (data portability rights) actually restore user agency if the coordination problem (finding where to migrate to) remains unsolved?',
    'Post-GDPR implementation analysis: measure proportion of users who export data and successfully migrate to alternative services versus those who export but never use the data. Track cost barriers (time to configure alternative, training, data format incompatibility).',
    'If data portability alone restores agency: Regulatory Piton becomes functional (theater drops, classification could become Scaffold). If portability fails to overcome coordination barriers: interoperability mandates on API level are required, and purely regulatory approaches are insufficient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_sufficiency, empirical, 'Whether data portability alone enables meaningful exit').

omega_variable(
    decentralized_alternative_viability,
    'Can federated/decentralized social networks achieve critical mass sufficient to compete with incumbents, or do network effects guarantee permanent lock-in regardless of technical alternatives?',
    'Monitor growth trajectories of Mastodon, Bluesky, ActivityPub-based networks, and blockchain-based alternatives. Compare user retention and feature parity against network growth rates. Identify whether adoption accelerates (positive feedback) or plateaus (lock-in floor).',
    'If decentralized platforms achieve 10%+ market share within 5 years: network effects are not immutable, lock-in can be overcome, Open Source Coalition perspective validated (Scaffold outcome). If they plateau below 5%: network effects are the binding constraint, structural change requires legal or market intervention beyond technical innovation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_alternative_viability, empirical, 'Whether decentralized alternatives can achieve critical mass').

omega_variable(
    platform_cost_of_lock_in_maintenance,
    'What proportion of platform operational cost is devoted to maintaining lock-in (API changes, deprecation cycles, data format proprietary constraints) versus genuine infrastructure provision?',
    'Platform engineering audit: measure engineering effort spent on interoperability features and data export versus effort spent on lock-in maintenance. Compare against platforms operating with weaker lock-in (email services, web protocols).',
    'If lock-in maintenance represents >40% of operational cost: platforms have economic incentive to erect barriers (extraction is economically rational). If <20%: lock-in is a policy choice rather than a cost necessity, and regulatory intervention has lower collateral damage risk.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_cost_of_lock_in_maintenance, empirical, 'Economic cost of maintaining versus removing lock-in mechanisms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_ownership_erosion, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digow_tr_t0, digital_ownership_erosion, theater_ratio, 0, 0.38).
narrative_ontology:measurement(digow_tr_t10, digital_ownership_erosion, theater_ratio, 10, 0.48).
narrative_ontology:measurement(digow_tr_t20, digital_ownership_erosion, theater_ratio, 20, 0.58).
narrative_ontology:measurement(digow_tr_t5, digital_ownership_erosion, theater_ratio, 5, 0.43).
narrative_ontology:measurement(digow_tr_t15, digital_ownership_erosion, theater_ratio, 15, 0.53).

% Extraction over time
narrative_ontology:measurement(digow_be_t0, digital_ownership_erosion, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(digow_be_t10, digital_ownership_erosion, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(digow_be_t20, digital_ownership_erosion, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(digow_be_t5, digital_ownership_erosion, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(digow_be_t15, digital_ownership_erosion, base_extractiveness, 15, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_ownership_erosion, resource_allocation).
narrative_ontology:affects_constraint(digital_ownership_erosion, platform_algorithmic_control).
narrative_ontology:affects_constraint(digital_ownership_erosion, data_extractivism).
narrative_ontology:affects_constraint(digital_ownership_erosion, interoperability_stickiness).

% DUAL FORMULATION NOTE:
% Digital ownership erosion decomposes into multiple structurally distinct constraints: (1) network effect coordination (genuine platform value), (2) API lock-in mechanisms (artificial barriers), (3) data extraction and asymmetric ownership (extraction asymmetry), (4) regulatory theater (piton maintenance). This story addresses the unified phenomenon; downstream constraints address specific mechanisms. Each has different ε values and different intervention points.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
