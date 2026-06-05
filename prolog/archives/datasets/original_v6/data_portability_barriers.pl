% ============================================================================
% CONSTRAINT STORY: data_portability_barriers
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_portability_barriers, []).

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
 *   constraint_id: data_portability_barriers
 *   human_readable: Data Portability Barriers in Digital Platforms
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   Data portability barriers represent a structural constraint where
 *   dominant digital platforms extract economic rent through user lock-in
 *   while simultaneously providing coordination services (network
 *   infrastructure, content curation, identity management). The constraint
 *   exhibits the full tangled-rope signature: genuine coordination function
 *   (platforms enable global communication) coexists with asymmetric
 *   extraction (users bear switching costs while platforms capture network
 *   effects). The barrier manifests across technical (incompatible data
 *   formats, API restrictions), legal (terms-of-service prohibitions on data
 *   exports), and behavioral (network lock-in, habit formation) mechanisms.
 *   Regulatory interventions (GDPR Article 20, DMA requirements) attempt to
 *   scaffold the constraint toward interoperability standards, but incumbent
 *   platforms have repeatedly adapted extraction mechanisms to circumvent
 *   portability mandates. The constraint's extractiveness has increased over
 *   the measurement interval (0.35 → 0.58) as platforms have developed more
 *   sophisticated lock-in mechanisms (algorithmic personalization, social
 *   graph dependency, integrated service ecosystems) despite regulatory
 *   portability requirements.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — locked in by network effects, data accumulation, algorithmic training, switching costs; experience maximum extraction
 *   - Competing Platforms: Organized victims (organized/constrained) — can access portability mechanisms but face coordinated suppression (API rate limiting, retroactive deprecation, contractual restrictions); experience asymmetric extraction during entry phase
 *   - Dominant Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract rent through lock-in while claiming coordination role; have high arbitrage options and capacity to modify rules
 *   - Regulatory Bodies: Organized enforcers (organized/constrained) — implement scaffold structures (GDPR, DMA) to mandate portability; constrained by political feasibility and technical implementation complexity
 *   - Enterprise Users: Secondary beneficiaries (powerful/mobile) — have sufficient bargaining power to negotiate direct data access; experience mixed extraction and coordination
 *   - Standards Organizations: Institutional coordinators (institutional/arbitrage) — develop interoperability standards (OAuth, data interchange formats) that reduce friction but lack enforcement mechanisms
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — identifies constraint as hybrid extraction-coordination mechanism that resists regulatory resolution through adaptation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_portability_barriers, 0.58).
domain_priors:suppression_score(data_portability_barriers, 0.65).
domain_priors:theater_ratio(data_portability_barriers, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_portability_barriers, extractiveness, 0.58).
narrative_ontology:constraint_metric(data_portability_barriers, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(data_portability_barriers, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_portability_barriers, tangled_rope).
narrative_ontology:human_readable(data_portability_barriers, "Data Portability Barriers in Digital Platforms").
narrative_ontology:topic_domain(data_portability_barriers, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(data_portability_barriers).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_portability_barriers, dominant_platforms).
narrative_ontology:constraint_beneficiary(data_portability_barriers, incumbent_service_providers).
narrative_ontology:constraint_victim(data_portability_barriers, user_switching_capacity).
narrative_ontology:constraint_victim(data_portability_barriers, competitive_market_efficiency).
narrative_ontology:constraint_victim(data_portability_barriers, consumer_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — User data accumulation over years creates switching costs that are structurally insurmountable for the individual: social graph, interaction history, trained algorithms, personalized content. Technical portability standards exist but lack practical force — exporting contact lists or posts does not reconstruct the network effect or algorithmic context. User is trapped by network dependency and data lock-in. Maximum extraction experienced through forced lock-in.
constraint_indexing:constraint_classification(data_portability_barriers, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING PLATFORM ENTRANTS (TANGLED ROPE) — New platforms benefit from portability mechanisms (data import APIs, OAuth integration) that enable rapid user acquisition, but face coordinated suppression from incumbent platforms (high API request costs, rate limiting, retroactive API deprecation, terms-of-service changes blocking third-party access). Genuine coordination function exists (standards like OAuth, GDPR data access rights) alongside asymmetric extraction (incumbents can block or monetize access). Constrained by both technical barriers and legal/contractual restrictions on API usage.
constraint_indexing:constraint_classification(data_portability_barriers, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: DOMINANT PLATFORM OPERATORS (ROPE) — Extraction flows toward incumbents, but platforms experience portability constraints as a pure coordination problem from their perspective: managing data export quality, ensuring API reliability, preventing abuse of bulk data access. Network effects and switching costs are perceived as natural coordination artifacts rather than extractive mechanisms. High arbitrage options — can leverage data access for monetization or simply deny it without market consequence.
constraint_indexing:constraint_classification(data_portability_barriers, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY MANDATE ENFORCERS (SCAFFOLD) — Data portability regulations (EU GDPR Article 20, DMA data access requirements, proposed bills in national jurisdictions) create temporary enforcement structures with explicit sunset logic: as interoperability standards mature and become costless, the regulatory mandate should become unnecessary. Seen as a scaffolding intervention to bootstrap competitive alternatives until markets self-organize. High theater during enforcement phase (compliance theater around data formats, timeliness, completeness) that declines as standards mature.
constraint_indexing:constraint_classification(data_portability_barriers, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY DATA SILOS (PITON) — Pre-smartphone data infrastructure (email, corporate databases, legacy social networks) maintains data lock-in through inertia despite reduced functional necessity. These systems persist because switching costs are high and institutional commitments are sunk, not because they actively serve coordination. Theater persists in the form of data export workflows and compliance theater, but the underlying extraction mechanism has weakened — users increasingly accept data loss as part of platform switching. Theater ratio high because the procedural export mechanisms create appearance of portability without enabling realistic switching.
constraint_indexing:constraint_classification(data_portability_barriers, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ENTERPRISE AND INSTITUTIONAL USERS (TANGLED ROPE) — Large organizations (corporations, universities, governments) have sufficient bargaining power to negotiate data portability terms directly with platforms, creating hybrid arrangements: genuine coordination through negotiated APIs and data sync protocols, alongside residual extraction (premium pricing for data access, contractual lock-in through volume discounts). Mobile exit options at institutional level but individual employees remain trapped. Organizations both benefit from and bear extraction costs depending on their scale and negotiating leverage.
constraint_indexing:constraint_classification(data_portability_barriers, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a systems perspective, data portability barriers simultaneously coordinate platform ecosystems (enabling specialization and vertical integration) while extracting rent from users through switching costs. The coordination function (platforms can offer specialized services because users can theoretically switch) is real but asymmetric: incumbents benefit disproportionately from reduced switching, while entrants and users bear extraction costs. The constraint is neither pure coordination nor pure extraction but a hybrid where incumbents have inverted the cost asymmetry to their advantage.
constraint_indexing:constraint_classification(data_portability_barriers, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_portability_barriers_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_portability_barriers, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_portability_barriers, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_portability_barriers, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_portability_barriers, TR),
    TR >= 0.70.

:- end_tests(data_portability_barriers_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and increasing. The extraction originates from genuine switching costs (network effects, algorithmic personalization, data accumulation over time) that are structurally real but asymmetrically distributed. Platforms benefit from switching costs; users bear them. The value reflects observed user reluctance to switch despite portability options and regulatory mandates — data exports exist but do not enable realistic switching because the coordination infrastructure (social graph, interaction history, personalized content) cannot be perfectly reconstructed. Extractiveness has increased from 0.35 to 0.58 over the interval despite regulatory pushes for portability, indicating that platforms have developed countermeasures (service degradation for exporting users, algorithmic de-prioritization, feature bundling) that prevent portability mandates from reducing lock-in. Suppression (0.65): High. Barriers to exit include technical complexity of data migration, psychological sunk costs, network dependency, contractual restrictions embedded in terms-of-service, and API access controls. Suppression is structural (network effects are real) but actively maintained (platforms invest in API restriction, rate limiting, format incompatibility). Theater ratio (0.48): Moderate. Portability workflows exist and create appearance of data freedom, but their practical effectiveness is limited — users perceive data export as possible but recognize that actual platform switching remains costly. Theater is lower than in legacy systems because users have learned that regulatory portability requirements do not translate to realistic switching capacity.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the dominant platform's Rope classification (sees portability as a coordination problem to solve) and the trapped user's Snare classification (experiences portability mandates as theater that does not enable realistic switching). The gap reveals the crux of the constraint: platforms claim that data portability solves the switching-cost problem, but users find that exportable data does not reconstruct the coordination infrastructure (social graph, algorithmic recommendations, interaction history, identity verification) that makes a platform valuable. The regulatory Scaffold perspective assumes that technical standards maturity will eventually make switching costless; the analytical Tangled Rope perspective recognizes that platforms can adapt extraction mechanisms faster than standards can close loopholes (algorithmic de-prioritization, service-tier differentiation, feature bundling). The gap between Rope and Snare, between regulation and extraction adaptation, is the diagnostic signal that the constraint is not solving but perpetuating.
 *
 * DIRECTIONALITY LOGIC:
 *   Dominant platforms benefit from data portability barriers (beneficiaries with arbitrage exit options → d ≈ 0.05 → f(d) ≈ -0.08 → negative χ). Individual users are trapped by these same barriers (victims with trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ). The chi formula χ = ε × f(d) × σ(S) correctly captures that the same barrier (network effect, data lock-in) produces negative extraction for beneficiaries and positive extraction for victims. The scope modifier σ(S) amplifies this asymmetry: at global scope (σ=1.2), platform benefits scale up while user costs also scale up — network effects are more powerful at global scale. This is the core directionality insight: portability barriers create asymmetric chi values for the same constraint because beneficiaries can exit (ignore portability) while victims cannot (trapped by network dependency).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy is resolved by recognizing that regulatory portability mandates create a genuine Scaffold intervention while the underlying Tangled Rope coordination function remains unchanged. The scaffold is real — regulatory enforcement of data access rights does temporarily reduce extraction as platforms must invest in compliance. But the scaffolding faces adaptive resistance: platforms develop secondary extraction mechanisms (algorithmic de-prioritization, service degradation, feature bundling) that are triggered when users export data. The theater ratio increase (0.32 → 0.48) reflects this adaptation — portability processes become more theatrical (users perceive export options but recognize limited utility) even as technical compliance improves. The mandatrophy is not resolved by choosing between Rope and Snare but by recognizing that regulation can reduce but not eliminate extraction because the coordination function itself (network effects, algorithmic personalization) remains the source of lock-in. Full resolution would require either (a) technical interoperability standards that eliminate network switching costs (Scaffold sunset), or (b) regulatory restructuring that decouples network effects from platform ownership (breaking the Tangled Rope into separate Rope components). Current trajectory suggests neither path is progressing quickly — extractiveness is increasing despite regulatory mandates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    portability_technical_vs_practical,
    'Is the observed portability gap technical (data exports are incomplete/incompatible) or behavioral (users psychologically cannot coordinate exit despite technical availability)?',
    'Direct user research: offer cohorts of users perfect technical data portability (pre-formatted, pre-imported, account setup automated) and measure actual switching rates. Compare against baseline switching without technical intervention.',
    'If technical: increasing technical standards maturity will reduce switching costs and decrease extractiveness. If behavioral: even perfect technical portability leaves extraction mechanism intact — users remain trapped by psychological switching costs and network effects. Classification may shift from Tangled Rope to Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(portability_technical_vs_practical, empirical, 'Whether portability gap is technical barriers or behavioral switching costs').

omega_variable(
    regulatory_extraction_substitution,
    'Does regulatory mandating of data portability reduce platform extraction or merely shift it to new mechanisms (pricing discrimination, service degradation, algorithmic de-prioritization for exporting users)?',
    'Longitudinal measurement of total user welfare (price paid, service quality, feature access) before and after portability mandate; statistical decomposition of extraction mechanisms before and after regulatory change.',
    'If extraction remains constant despite mandate: extraction mechanism is deep and adaptive (platforms can extract via alternative channels). Constraint reclassifies as Snare with theatrical compliance layer. If extraction decreases: mandate successfully disrupts extraction mechanism — Scaffold perspective validated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_extraction_substitution, empirical, 'Whether regulation reduces or displaces platform extraction').

omega_variable(
    network_effect_necessity,
    'Are observed network effects inherent to social platforms or largely produced by platform design choices (feed algorithms, matching, notifications, exclusivity incentives) that could be decoupled from data portability?',
    'Comparative analysis of platforms with different architectural choices: interoperable vs proprietary, algorithmic vs chronological feeds, open vs closed user graphs. Identify which network effects are structural vs contingent on design.',
    'If effects are contingent: portability barriers are extracted rent without genuine coordination function — constraint reclassifies toward Snare. If effects are structural: network effects are real coordination phenomena that justify some portability friction — constraint remains Tangled Rope with lower extractiveness ε.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_necessity, conceptual, 'Whether network effects are structural or design-contingent').

omega_variable(
    interoperability_convergence_timeline,
    'What timeline for technical and contractual interoperability standards would make regulatory scaffolding unnecessary?',
    'Technology roadmap analysis: identify dependencies in standards development (API specifications, data format standardization, authentication protocols). Project convergence timeline from current state to full interoperability.',
    'If convergence plausible within 5-10 years: Scaffold sunset clause is realistic. If convergence requires 20+ years or faces structural barriers: Scaffold is aspirational — extraction mechanism may persist indefinitely despite regulatory intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_convergence_timeline, empirical, 'Timeline to technical interoperability maturity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_portability_barriers, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(datport_tr_t0, data_portability_barriers, theater_ratio, 0, 0.32).
narrative_ontology:measurement(datport_tr_t3, data_portability_barriers, theater_ratio, 3, 0.42).
narrative_ontology:measurement(datport_tr_t6, data_portability_barriers, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(datport_be_t0, data_portability_barriers, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(datport_be_t3, data_portability_barriers, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(datport_be_t6, data_portability_barriers, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_portability_barriers, global_infrastructure).
narrative_ontology:affects_constraint(data_portability_barriers, network_effect_lock_in).
narrative_ontology:affects_constraint(data_portability_barriers, algorithmic_recommendation_dependence).
narrative_ontology:affects_constraint(data_portability_barriers, platform_ecosystem_integration).

% DUAL FORMULATION NOTE:
% Data portability barriers decompose into three structural constraints: (1) technical interoperability (data format compatibility, API access) — ε ≈ 0.30, primarily coordination problem; (2) network effect switching costs (social graph reconstruction, algorithmic training on new platform) — ε ≈ 0.62, primarily extraction problem; (3) regulatory scaffolding effectiveness (whether portability mandates reduce actual switching costs) — ε ≈ 0.58, the constraint in this story. The three stories are linked: regulatory efforts target (1) but cannot eliminate (2) without technological or structural changes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
