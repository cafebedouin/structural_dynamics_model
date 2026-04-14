% ============================================================================
% CONSTRAINT STORY: platform_gatekeeper_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_gatekeeper_power, []).

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
 *   constraint_id: platform_gatekeeper_power
 *   human_readable: Platform Gatekeeper Power
 *   domain: digital_economy/platform_governance
 *
 * SUMMARY:
 *   Platform gatekeeper power emerges as a structural constraint when digital
 *   platforms achieve sufficient network effects to become quasi-monopolistic
 *   intermediaries between creators, merchants, and consumers. The constraint
 *   operates through a combination of genuine coordination (platforms solve
 *   the problem of matching supply and demand at scale) and extractive
 *   lock-in (network effects and switching costs trap dependent actors). The
 *   constraint's classification varies radically across perspectives: from
 *   pure extraction (Snare) for creators whose entire income depends on a
 *   single platform, to pure coordination (Rope) from the platform operator's
 *   perspective, to a mixed coordination-extraction hybrid (Tangled Rope)
 *   from organized creators and consumers. The theater ratio's rise from 0.30
 *   to 0.55 reflects the accumulation of performative regulation (privacy
 *   policies, content moderation appeals, community guidelines) that operate
 *   as public relations without corresponding functional governance. The
 *   extractiveness progression from 0.35 to 0.58 reflects two trends: (1)
 *   platform deepening of commissions and fees as competitive pressure
 *   decreases, and (2) growth of organizing among dependent actors, raising
 *   the measured extraction from the platform operator's perspective
 *   (organized agents pay higher effective extraction than individual
 *   dependents).
 *
 * KEY AGENTS:
 *   - Platform Operator: Institutional beneficiary (institutional/arbitrage) — captures network value, controls algorithmic visibility, sets unilateral terms
 *   - Dependent Creator: Primary victim (powerless/trapped) — entire income and audience bound to platform, zero negotiating power
 *   - Dependent Merchant: Primary victim (powerless/trapped) — customer access and inventory visibility entirely platform-mediated
 *   - Organized Creator Coalition: Secondary victim (moderate/constrained) — collective bargaining power but constrained by audience fragmentation and switching costs
 *   - Consumer Base: Mixed victim (moderate/constrained) — benefits from coordination but bears extraction through price discrimination, data extraction, and reduced choice
 *   - Interoperability Coalition: Organized agents (organized/constrained) — regulatory bodies and alternative protocol initiatives building exit pathways and sunset mechanisms
 *   - Content Moderation System: Institutional actor (institutional/arbitrage) — performative governance theater maintained through inertia and regulatory pressure
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing network effects as technological inevitability rather than policy artifact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_gatekeeper_power, 0.58).
domain_priors:suppression_score(platform_gatekeeper_power, 0.68).
domain_priors:theater_ratio(platform_gatekeeper_power, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_gatekeeper_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_gatekeeper_power, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_gatekeeper_power, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_gatekeeper_power, tangled_rope).
narrative_ontology:human_readable(platform_gatekeeper_power, "Platform Gatekeeper Power").
narrative_ontology:topic_domain(platform_gatekeeper_power, "digital_economy/platform_governance").

domain_priors:requires_active_enforcement(platform_gatekeeper_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_gatekeeper_power, platform_operator).
narrative_ontology:constraint_victim(platform_gatekeeper_power, dependent_creators).
narrative_ontology:constraint_victim(platform_gatekeeper_power, dependent_merchants).
narrative_ontology:constraint_victim(platform_gatekeeper_power, consumer_choice).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT CREATOR (SNARE) — Creator whose entire income and audience derive from a single platform. Cannot exit without losing livelihood, audience, and years of accumulated content value. Platform enforces algorithmic ranking, content moderation, commission structures with zero transparency or appeal. Maximum extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(platform_gatekeeper_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEPENDENT MERCHANT (SNARE) — Small retailer whose sales, inventory visibility, and customer access flow entirely through a platform. Trapped by customer distribution: migrating to alternative platform means losing established customer relationships. Platform dictates fees, product policies, and visibility criteria unilaterally. Suppression is structural — no alternative distribution channels at comparable scale.
constraint_indexing:constraint_classification(platform_gatekeeper_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ORGANIZED CREATOR COALITION (TANGLED ROPE) — Collective of creators with negotiating power (union, guild, trade association) experiences the platform as both coordinator and extractor. The platform solves genuine coordination problems: matching creators with audiences at scale, managing payment infrastructure, hosting content. But the platform also extracts through commission asymmetry, algorithmic opacity, and unilateral policy changes. Organized agents have some exit options (multi-platform strategies, direct funding) but face switching costs and audience fragmentation.
constraint_indexing:constraint_classification(platform_gatekeeper_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — The platform benefits from the constraint as pure coordination: aggregating supply, matching with demand, managing trust and payments. Experiences the gatekeeper power as the legitimate function of the platform itself. Exit options include expanding to new markets, adding services, optimizing fees. Extraction runs toward this agent, not away — they are the primary beneficiary of the asymmetry.
constraint_indexing:constraint_classification(platform_gatekeeper_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER BASE (TANGLED ROPE) — Consumers benefit from platform coordination (selection, price comparison, trust ratings) but also bear extraction costs through platform rent-seeking, price discrimination, and data extraction. Constrained by network effects: switching to alternative platform means losing access to incumbent creators/merchants and their audience/inventory. Some organizational power (consumer advocacy) but high switching costs.
constraint_indexing:constraint_classification(platform_gatekeeper_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTEROPERABILITY COALITION (SCAFFOLD) — Regulatory bodies, open-source projects, and alternative platform initiatives (fediverse, decentralized protocols, digital markets acts) see gatekeeper power as a temporary structural problem with a sunset clause. Digital Markets Act, data portability requirements, and interoperability standards are building technical and regulatory pathways to reduce platform lock-in. This is a genuine sunset: as interoperability matures, the gatekeeper's extraction mechanism (network effect + switching cost + switching cost) loses force.
constraint_indexing:constraint_classification(platform_gatekeeper_power, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY CONTENT MODERATION THEATER (PITON) — Platform's content moderation and community guidelines have devolved into largely performative activity: opaque appeal processes, inconsistent enforcement, reliance on algorithmic systems that generate false positives at scale. The theater persists due to institutional inertia and regulatory pressure rather than actual effective governance. The function (community trust) has atrophied; the ritual (moderation boards, policy documents) remains. Theater ratio is high because most visibility of moderation is public relations rather than actual justice.
constraint_indexing:constraint_classification(platform_gatekeeper_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, platform gatekeeping appears to be an immutable consequence of digital coordination at scale: any system that connects many producers to many consumers must have some curatorial or filtering function, and that function grants structural power to whoever controls it. This perspective risks naturalizing what is actually a contingent institutional arrangement (network effects as a technological barrier rather than a policy choice). False summit detection will flag this.
constraint_indexing:constraint_classification(platform_gatekeeper_power, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_gatekeeper_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_gatekeeper_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_gatekeeper_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_gatekeeper_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_gatekeeper_power, TR),
    TR >= 0.70.

:- end_tests(platform_gatekeeper_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract through multiple channels: commission rates (15-30% typical for e-commerce, 30-50% for app stores, variable for content creators), algorithmic visibility control, unilateral policy changes, and data monetization. However, genuine coordination value is significant — platforms do solve the matching and trust problems at scale. Extractiveness of 0.58 reflects that extraction is substantial but not maximal; genuine coordination functions are present. Suppression (0.68): High. Multiple suppression mechanisms operate: (1) network effects create lock-in (switching away means abandoning audience/customer base), (2) alternative platforms fragment supply/demand rather than replace (a creator cannot move their audience wholesale), (3) no transparent appeal or governance mechanisms for policy disputes, (4) technical barriers (content export, audience portability) are non-trivial. Suppression is high because exit options are genuinely constrained, not merely costly. Theater ratio (0.55): Moderate-high. Content moderation, community guidelines, and appeals processes are substantially performative: opaque decision-making, inconsistent enforcement, algorithmic false positives, and lack of meaningful recourse. But platforms do perform some genuine trust/safety functions. Theater increased from 0.30 to 0.55 as regulation pressured platforms to create visible governance theater (bias audits, transparency reports, policy frameworks) while actual enforcement remained opaque and algorithmic.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the platform operator's Rope and the dependent creator's Snare reveals the asymmetry of the constraint: the beneficiary experiences coordination, the victim experiences extraction. The analytical observer's Mountain risks naturalizing this asymmetry as technological inevitability ('platforms must have gatekeepers') rather than as a policy choice (interoperability could distribute the gatekeeper function). The organized creators' Tangled Rope sits between these extremes, reflecting mixed benefits and extraction. The Scaffold perspective's sunset logic is crucial: if interoperability succeeds, switching costs collapse, and dependent actors gain genuine exit options, transforming the classification for all victim perspectives from Snare/Tangled Rope toward Rope or Scaffold. The perspectival gap thus encodes the political economy of the constraint: current gatekeeping is extractive, but future interoperability could restore coordination-only dynamics.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary (platform operator) with arbitrage exit: derives d ≈ 0.12 from institutional power + arbitrage options + beneficiary status. Low d produces negative f(d) and negative χ from their perspective, so they classify the constraint as Rope (pure coordination) — the extraction runs toward them but is not their experienced burden. Victims (dependent creators/merchants) with trapped exit: derive d ≈ 0.92 from powerless power + trapped options + victim status. High d produces maximum f(d) ≈ 1.42 and high χ from their perspective, producing Snare (pure extraction). Organized creators with constrained exit: derive d ≈ 0.60 from moderate power + constrained options + mixed victim/partial-beneficiary status. Middle d produces moderate f(d) ≈ 0.92 and moderate χ, enabling Tangled Rope (mixed coordination/extraction). The directionality values are derived from the structural relationship declarations (beneficiaries: platform_operator; victims: dependent_creators, dependent_merchants, consumer_choice) combined with the exit options for each perspective. No overrides needed — the structural data is sufficient to produce differentiated directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination from extraction via the Tangled Rope category. The initial risk is that platforms appear to be pure coordination (matching creators with audiences, solving the discovery problem, providing infrastructure) and thus should classify as Rope from all perspectives. But the perspective-specific classification reveals the asymmetry: the platform operator genuinely experiences Rope (coordination is their entire function), while dependent actors experience Snare (pure extraction relative to their position). The Tangled Rope from the organized coalition's perspective confirms that both coordination AND asymmetric extraction are present in the same constraint structure — it's not a matter of perspective illusion but of structural asymmetry. The mandatrophy is resolved by the indexical tuple: (powerless, biographical, trapped, global) sees Snare; (institutional, immediate, arbitrage, global) sees Rope; (moderate, biographical, constrained, national) sees Tangled Rope. All three are structurally correct descriptions of the same ε = 0.58 constraint. The platform is simultaneously a coordination technology (genuine) and an extraction mechanism (genuine). Which one is 'primary' depends on the observer's position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_contingency,
    'Are network effects (value increases with user count) a fundamental property of digital platforms or a contingent outcome of specific architectural and policy choices?',
    'Comparative analysis of platforms with weak network effects (decentralized protocols, interoperable services); testing whether interoperability reduces lock-in effects; measurement of switching costs pre- and post-interoperability policy implementation',
    'If fundamental: gatekeeper power is unavoidable (mountain). If contingent: gatekeeper power is a policy artifact, and interoperability can dissolve it structurally. Classification shifts from Mountain to Tangled Rope or Scaffold depending on policy trajectory.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_contingency, conceptual, 'Whether network effects are fundamental or contingent').

omega_variable(
    extraction_floor_for_platform_coordination,
    'What minimum commission/fee rate would constitute legitimate coordination cost vs. extractive overhead for platform services (payment processing, hosting, discovery)?',
    'Cost accounting analysis: actual infrastructure costs, payment processing fees, content delivery costs, moderation overhead, compared against platform commission rates across different market segments',
    'If extraction floor is 5-10%: most platforms operate within reasonable coordination cost. If floor is 2-3%: current platform commissions (15-30%) are predominantly extractive rent-seeking. Classification implications: higher extraction floor (Rope or Tangled Rope) vs. lower extraction floor (Snare or more extractive Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_floor_for_platform_coordination, empirical, 'Legitimate coordination cost floor for platform services').

omega_variable(
    exit_option_feasibility_for_creators,
    'Can creators realistically establish independent audience distribution channels (direct-to-fan, alternative platforms, federated protocols) at comparable scale to platform-dependent distribution within a 3-year horizon?',
    'Longitudinal tracking of creator migration success rates; measurement of audience retention and growth rates when creators establish independent channels; comparative engagement metrics on platform vs. independent channels',
    'If feasible: creator exit options upgrade from ''trapped'' to ''constrained'' or ''mobile''. Classifications shift from Snare to Tangled Rope or Rope. If not feasible: trapped status is confirmed, Snare classification holds for dependent creators.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_feasibility_for_creators, empirical, 'Whether creators can realistically migrate to independent channels').

omega_variable(
    interoperability_implementation_timeline,
    'How long until Digital Markets Act interoperability requirements, data portability, and federated protocol adoption actually reduce platform switching costs from current 80%+ to levels where exit becomes genuinely feasible (target: <30%)?',
    'Policy implementation tracking; technical readiness assessment of interoperability standards; measurement of actual switching costs post-implementation; creator/merchant migration rates when exit options improve',
    'If timeline is 2-5 years: Scaffold classification is realistic (sunset visible). If timeline is 10+ years: Scaffold is aspirational, and constraints remain Snare/Tangled Rope. If implementation stalls: Scaffold reverts to Piton (performative regulation without effect).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_implementation_timeline, empirical, 'Timeline for interoperability to reduce switching costs materially').

omega_variable(
    algorithmic_transparency_sufficiency,
    'Does transparency about algorithmic ranking (disclosing ranking factors, showing how content is prioritized) constitute meaningful exit opportunity (creator can optimize) or merely theater (ranking remains opaque in practice, optimization is captured by platform)?',
    'Comparison of creator success metrics pre- and post-transparency disclosure; analysis of whether disclosed ranking factors actually predict visibility; measurement of whether creators can successfully optimize for disclosed metrics or whether platform changes criteria in response',
    'If transparency enables real optimization: exit options improve for organized creators, classification shifts toward Rope. If optimization is captured/futile: transparency is theater (Piton-level performance), and trapped status confirmed for dependent creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_sufficiency, empirical, 'Whether algorithmic transparency enables meaningful creator agency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_gatekeeper_power, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pgp_tr_t0, platform_gatekeeper_power, theater_ratio, 0, 0.3).
narrative_ontology:measurement(pgp_tr_t3, platform_gatekeeper_power, theater_ratio, 3, 0.42).
narrative_ontology:measurement(pgp_tr_t6, platform_gatekeeper_power, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(pgp_be_t0, platform_gatekeeper_power, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pgp_be_t3, platform_gatekeeper_power, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(pgp_be_t6, platform_gatekeeper_power, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_gatekeeper_power, resource_allocation).
narrative_ontology:affects_constraint(platform_gatekeeper_power, algorithmic_visibility_opacity).
narrative_ontology:affects_constraint(platform_gatekeeper_power, creator_financial_precarity).
narrative_ontology:affects_constraint(platform_gatekeeper_power, platform_monopoly_consolidation).

% DUAL FORMULATION NOTE:
% Platform gatekeeper power should be decomposed into three structurally distinct constraints: (1) algorithmic visibility control (ε ≈ 0.50, Tangled Rope — genuine ranking function + opaque optimization), (2) commission structure extraction (ε ≈ 0.65, Snare — pure rent extraction from dependent merchants), (3) data extraction and monetization (ε ≈ 0.55, Tangled Rope — coordination of personal data markets + asymmetric information advantage). This story represents the aggregate constraint; each component has different manipulation vectors and different sunset opportunities.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
