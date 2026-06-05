% ============================================================================
% CONSTRAINT STORY: switching_cost_economies
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_switching_cost_economies, []).

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
 *   constraint_id: switching_cost_economies
 *   human_readable: Switching Cost Economies and Lock-in Extraction
 *   domain: economic/organizational/digital
 *
 * SUMMARY:
 *   Switching cost economies represent a fundamental structural constraint in
 *   network-dependent systems where users, businesses, or organizations
 *   accumulate data, relationships, integrations, or reputation that becomes
 *   costly to migrate. This constraint operates across digital platforms
 *   (social media, cloud services, productivity software), financial systems
 *   (payment processors, banking platforms), supply chains (integrated
 *   manufacturing systems), and organizational ecosystems (enterprise
 *   software, healthcare records). The constraint exhibits high perspectival
 *   variance: platform operators experience it as a coordination mechanism
 *   enabling long-term relationship investment; locked-in users experience it
 *   as pure extraction; regulators experience it as a temporary problem being
 *   solved through interoperability mandates; and large enterprises with
 *   bargaining power experience it as a mixed coordination-extraction hybrid.
 *   The constraint's theater ratio (0.48) remains relatively low throughout
 *   the measurement interval because switching costs operate through genuine
 *   technical coupling (data integration, API depth, network effects) rather
 *   than through performative mechanisms. However, extractiveness has risen
 *   from 0.32 to 0.58 over the interval, indicating that platform operators
 *   have increasingly weaponized technical switching costs to extract
 *   additional rents through price increases, forced feature adoption, and
 *   algorithmic degradation of user experience.
 *
 * KEY AGENTS:
 *   - Trapped Users: Primary victims (powerless/trapped) — individuals whose data, social graphs, financial history, or workflow integration is embedded in a platform; face insurmountable barriers to exit
 *   - Locked-in Businesses: Secondary victims (moderate/constrained) — small businesses, developers, content creators built on platform infrastructure; face severe but not insurmountable exit costs; also benefit from coordination function
 *   - Incumbent Platform Operators: Primary beneficiaries (institutional/arbitrage) — Amazon Web Services, Meta, Apple, Microsoft; capture switching cost rents through price increases and forced compliance; can exit constraint entirely by reducing switching friction (choice not to)
 *   - Regulatory Coalitions: Organized agents (organized/mobile) — antitrust authorities (DOJ, EU Commission), data privacy regulators; building interoperability mandates and data portability rights to reduce switching friction; see sunset clause in regulatory timelines
 *   - Legacy System Operators: Institutional inertial actors (institutional/arbitrage) — older platforms and formats where switching costs persist through compatibility requirements and IP enforcement despite diminished functional purpose
 *   - Large Enterprises: Powerful agents (powerful/mobile) — Fortune 500 companies that can negotiate custom terms, run parallel systems, or invest in migration; experience mixed coordination and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(switching_cost_economies, 0.58).
domain_priors:suppression_score(switching_cost_economies, 0.62).
domain_priors:theater_ratio(switching_cost_economies, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(switching_cost_economies, extractiveness, 0.58).
narrative_ontology:constraint_metric(switching_cost_economies, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(switching_cost_economies, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(switching_cost_economies, tangled_rope).
narrative_ontology:human_readable(switching_cost_economies, "Switching Cost Economies and Lock-in Extraction").
narrative_ontology:topic_domain(switching_cost_economies, "economic/organizational/digital").

domain_priors:requires_active_enforcement(switching_cost_economies).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(switching_cost_economies, incumbent_providers).
narrative_ontology:constraint_beneficiary(switching_cost_economies, platform_operators).
narrative_ontology:constraint_victim(switching_cost_economies, locked_in_users).
narrative_ontology:constraint_victim(switching_cost_economies, dependent_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED USER (SNARE) — Users whose data, social graphs, workflows, or financial commitments are embedded in a platform face insurmountable barriers to exit. Cannot retrieve accumulated value; faces social isolation if leaving (network effects); bears extraction through price increases, degraded service quality, or forced feature adoption. Maximum suppression and no real alternatives.
constraint_indexing:constraint_classification(switching_cost_economies, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT BUSINESS (TANGLED ROPE) — Small businesses built on a platform (Shopify seller, AWS startup, app developer on iOS) experience genuine coordination benefits (reach, infrastructure, payment processing) alongside extraction (fee increases, algorithm changes, forced compliance with new requirements). Suppression is high (costs of exit are severe) but not total — some businesses successfully migrate. Mixed extraction and coordination.
constraint_indexing:constraint_classification(switching_cost_economies, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PLATFORM OPERATOR (ROPE) — Experiences the switching cost structure as a coordination mechanism: high retention enables long-term relationship investment, predictable revenue, ecosystem stability. Net beneficiary. Can exit the constraint entirely by reducing switching costs (though they don't), making this a pure governance choice, not a structural trap. Arbitrage option (could switch business models) makes exit real even if unexercised.
constraint_indexing:constraint_classification(switching_cost_economies, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Antitrust authorities, data portability regulations (GDPR), interoperability mandates, and open standards initiatives (DMA, Digital Markets Act) see switching costs as a temporary problem being actively solved. Sunset clause: data portability rights, open APIs, and portability standards are reducing switching friction. High agency for this actor; sees a clear exit path through regulatory intervention. Theater is low here — enforcement action is substantive, not ritual.
constraint_indexing:constraint_classification(switching_cost_economies, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 5: LEGACY COMPATIBILITY STANDARD (PITON) — Older switching cost mechanisms (proprietary file formats, closed ecosystems that predate cloud computing) persist through institutional inertia. The constraint has become less functional as cloud platforms and open standards reduce the actual lock-in, but ritual compliance (maintaining backward compatibility, continuing to enforce IP restrictions) keeps the constraint alive artificially. Theater ratio high because the functional purpose has atrophied.
constraint_indexing:constraint_classification(switching_cost_economies, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LARGE ENTERPRISE (TANGLED ROPE) — Powerful agents (Fortune 500 companies, major institutions) face switching costs but can negotiate custom terms, run parallel systems, or invest in migration. Experience genuine coordination benefits (economies of scale, integration depth, vendor support) and asymmetric extraction (volume discounts offset by lock-in leverage). Mobile exit option because they have resources; beneficiary status because they shape the relationship through bargaining power. Mixed experience with moderate extraction.
constraint_indexing:constraint_classification(switching_cost_economies, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a naive economic view, switching costs appear as an immutable feature of any network system: integrating new participants into an established network always requires investment. This perspective risks naturalizing what is actually a contingent design choice. The engine should flag this as a false summit — switching costs are not laws of physics but policy decisions (API design, data portability, interoperability mandates).
constraint_indexing:constraint_classification(switching_cost_economies, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(switching_cost_economies_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(switching_cost_economies, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(switching_cost_economies, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(switching_cost_economies, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(switching_cost_economies, TR),
    TR >= 0.70.

:- end_tests(switching_cost_economies_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts significant value from locked-in users and dependent businesses through price premiums, reduced service quality for captive users, forced adoption of new features, and reduced innovation pressure (because users cannot easily switch). However, extractiveness is not extreme (not 0.72+) because: (1) some users and businesses do successfully migrate despite high costs, indicating suppression is not total; (2) regulatory pressure (GDPR, DMA) is beginning to reduce switching friction; (3) open-source alternatives and competing platforms provide imperfect but real alternatives; (4) the constraint has genuine coordination benefits (users receive real value from the platform, not pure extraction). The measurement trajectory (0.32 → 0.58 over 15 years) reflects how platform operators have increasingly weaponized technical coupling for extraction, particularly after achieving market dominance. Suppression (0.62): High. Users and dependent businesses face multiple barriers to exit: (a) data retrieval and migration costs; (b) social network effects (leaving requires others to leave); (c) learning curve and workflow reintegration costs; (d) financial commitments (prepaid contracts, sunk integration investments); (e) regulatory and compliance barriers in some sectors. But suppression is not total (0.9+) because some migrations do occur and regulatory interventions are reducing barriers. Theater ratio (0.48): Moderate. Switching costs operate primarily through genuine technical integration (deep API coupling, data architecture dependencies) rather than through performative mechanisms. However, some theatricality exists: (1) platforms exaggerate switching cost claims (create cultural narratives that switching is 'too hard'); (2) compatibility theater (claiming to support open standards while maintaining de facto lock-in); (3) migration tool theater (offering migration assistants that are technically incomplete or deliberately slow).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates substantial perspectival divergence across the observation field. The platform operator (institutional/arbitrage) sees coordination and sustainability (Rope perspective) — they genuinely need switching costs to fund long-term infrastructure investment and relationship maintenance. The trapped user (powerless/trapped) sees pure extraction (Snare perspective) — they are locked in with no alternatives and bear all costs. The dependent business (moderate/constrained) sees mixed effects (Tangled Rope perspective) — the platform provides genuine value (payment processing, reach, infrastructure) but also extracts through rent-seeking after lock-in. The regulatory coalition (organized/mobile) sees a temporary problem with a clear sunset (Scaffold perspective) — interoperability mandates and data portability rights are actively reducing switching friction. The large enterprise (powerful/mobile) sees a negotiated hybrid (Tangled Rope perspective from powerful position) — they experience extraction but have bargaining leverage to reduce it. The legacy system operator (institutional/arbitrage) sees a degraded ritual (Piton perspective) — compatibility requirements and IP restrictions persist through inertia despite technical obsolescence. The analytical observer (analytical/analytical) risks naturalizing contingent design choices as laws of network economics (false Mountain perspective). The perspectival gap reveals that 'switching costs' is not a single phenomenon but a portfolio of structural mechanisms experienced differently depending on power, exit options, and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality pipeline computes d from: (1) beneficiary/victim status; (2) power level; (3) exit options; (4) time horizon. Beneficiaries (platform operators) with arbitrage exit options get low d (0.10-0.20) because they can exit and are extracting. Victims (locked-in users) with trapped exit get high d (0.90-0.95) because they cannot escape the extraction. Constrained exit (dependent businesses) gets intermediate d (0.65-0.75) because exit is possible at high cost. Mobile exit (regulated platforms, large enterprises) gets moderate d (0.40-0.60) because exit is feasible though difficult. The sigmoid function f(d) converts these d values into effective power modifiers that determine how much of the base extractiveness (ε) actually manifests as perceived extraction (χ) from that perspective. Institutional power atoms with beneficiary status tend to get negative f(d) values (institutional extraction reversed from the agent's perspective — they see themselves as providers, not extractors). Powerless agents with victim status get f(d) > 1.0 (maximum perceived extraction). The directionality derives from structural facts (who benefits, who can exit) rather than subjective perception — it is objective even though it varies by index.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that switching cost economies combine genuine coordination (platforms provide real technical and ecosystem benefits) with asymmetric extraction (operators capture lock-in rents). The Tangled Rope classification prevents misclassification in two directions: (1) prevents treating this as pure coordination (Rope) by showing the asymmetric extraction and suppression; (2) prevents treating this as pure extraction (Snare) by showing the genuine coordination function and the mixed experience of most agents. The engine's mandatory multi-perspective requirement reveals the perspectival divergence: what looks like Rope from the platform operator's vantage point looks like Snare from the trapped user's vantage point, and this gap is not measurement error — it is structural. The constraint is a genuine hybrid. The regulatory coalition's Scaffold classification adds temporal dimension: the sunset clause (interoperability mandates, data portability rights) is real and enforceable, distinguishing this from a piton (which has no functional sunset). The presence of multiple beneficiaries and victims, combined with high suppression and moderate-high extractiveness, satisfies the Tangled Rope gates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_legitimacy_threshold,
    'What level of switching cost represents legitimate coordination overhead versus extractive lock-in rent?',
    'Comparative analysis: switching costs in competitive markets vs. monopolistic platforms; correlation between switching friction and pricing power; study of user welfare under different switching cost regimes',
    'If threshold is high: many lock-in mechanisms reclassified as legitimate coordination costs (Rope from more perspectives). If threshold is low: more constraints classified as extractive (Snare from more perspectives). Regulatory design of data portability depends critically on this threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_legitimacy_threshold, empirical, 'Threshold distinguishing legitimate switching costs from extractive lock-in').

omega_variable(
    interoperability_sufficiency,
    'Can true interoperability (open APIs, data portability, format neutrality) actually reduce switching costs to competitive market levels, or do network effects and coordination depth create inherent stickiness?',
    'Empirical study of markets with mandatory interoperability (EU telecom roaming, SWIFT interoperability); measurement of actual user switching rates pre- and post-interoperability mandate; analysis of whether switching still fails to occur even when technically possible',
    'If interoperability sufficient: scaffold sunset is real — regulatory intervention can solve the constraint structurally. If insufficient: switching costs persist despite open standards (more of a piton than a scaffold) because of coordination depth and network effects, not technical barriers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interoperability_sufficiency, empirical, 'Whether interoperability mandates can reduce switching costs to competitive levels').

omega_variable(
    user_awareness_and_consent,
    'Do users actually understand the switching costs they are incurring at adoption? Is lock-in an informed trade-off or a hidden structural trap?',
    'User surveys measuring switching cost awareness at adoption; comparison of stated switching costs vs. revealed switching costs (willingness to pay to switch); analysis of terms-of-service transparency regarding lock-in mechanisms',
    'If high awareness and informed consent: switching cost structure is more Rope than Snare (coordination chosen voluntarily). If low awareness and hidden traps: constraint is more Snare than Rope (extraction through information asymmetry). Suppression value depends on this distinction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_awareness_and_consent, empirical, 'Whether users understand and consent to switching costs at adoption').

omega_variable(
    platform_switching_cost_design_intentionality,
    'Are high switching costs a byproduct of legitimate technical integration or deliberate design choices by platform operators to increase extraction?',
    'Platform design analysis: comparison of switching friction under different technical architectures; study of platforms that deliberately reduce switching costs (Stripe, Twilio) vs. platforms that maximize friction; historical analysis of API design decisions and their documented rationale',
    'If intentional design: beneficiary status of platform operators is clearer (they chose extraction), and the constraint should be classified as more extractive from sandboxed analyses. If technical byproduct: more ambiguity in whether platform has genuine coordination intent or is exploiting accidental lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_switching_cost_design_intentionality, empirical, 'Whether platform switching costs are intentionally designed or technical byproducts').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(switching_cost_economies, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(switch_tr_t0, switching_cost_economies, theater_ratio, 0, 0.32).
narrative_ontology:measurement(switch_tr_t8, switching_cost_economies, theater_ratio, 8, 0.4).
narrative_ontology:measurement(switch_tr_t15, switching_cost_economies, theater_ratio, 15, 0.48).
narrative_ontology:measurement(switch_tr_t5, switching_cost_economies, theater_ratio, 5, 0.36).

% Extraction over time
narrative_ontology:measurement(switch_be_t0, switching_cost_economies, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(switch_be_t8, switching_cost_economies, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(switch_be_t15, switching_cost_economies, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(switch_be_t5, switching_cost_economies, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(switching_cost_economies, resource_allocation).
narrative_ontology:affects_constraint(switching_cost_economies, network_effects_lock_in).
narrative_ontology:affects_constraint(switching_cost_economies, data_portability_enforcement).
narrative_ontology:affects_constraint(switching_cost_economies, interoperability_mandate_implementation).

% DUAL FORMULATION NOTE:
% Switching cost economies decompose into multiple structural constraints: (1) technical integration depth (genuine coordination cost), (2) network effects (emergent lock-in from user behavior), (3) data architecture coupling (path-dependent integration), (4) policy design choices (deliberate lock-in mechanisms). This story addresses the aggregate constraint. The network links show how regulatory action on interoperability and data portability directly reduce switching costs, transforming the classification trajectory over time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(switching_cost_economies, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
