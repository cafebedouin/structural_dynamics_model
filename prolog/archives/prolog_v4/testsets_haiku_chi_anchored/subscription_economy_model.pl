% ============================================================================
% CONSTRAINT STORY: subscription_economy_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subscription_economy_model, []).

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
 *   constraint_id: subscription_economy_model
 *   human_readable: The Subscription Economy Model
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The subscription economy model represents a structural shift in software
 *   and digital service delivery from perpetual ownership (one-time purchase)
 *   to recurring rentals. Originally justified as a coordination mechanism
 *   enabling continuous updates and support, the model has evolved into a
 *   primary extraction mechanism. Software vendors benefit from locked-in
 *   recurring revenue; consumers are locked into dependency without exit
 *   options. The constraint exhibits all characteristics of a Tangled Rope:
 *   genuine coordination function (automatic updates, cloud infrastructure,
 *   feature development funding) combined with severe asymmetric extraction
 *   (price escalation, vendor lock-in through proprietary APIs, deprecation
 *   of perpetual license alternatives). The theater ratio (0.55) reflects
 *   that subscription mechanisms include significant performative overhead:
 *   licensing servers, usage tracking, account management systems, and
 *   'always online' requirements that are technically redundant but enforced
 *   for extraction verification. The extractiveness has increased from 0.15
 *   to 0.52 over the 10-year interval as vendors progressively eliminated
 *   perpetual license alternatives and engineered deeper lock-in through
 *   cloud integration and proprietary data formats. Open-source alternatives
 *   (Linux, LibreOffice, Blender, GIMP) represent a genuine scaffold
 *   structure — organized agents building alternative pathways with a
 *   plausible sunset as open-source software reaches feature parity in more
 *   domains.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary victim (powerless/trapped) — dependent on subscription software with no perpetual license alternative; cannot exit without significant disruption
 *   - Small Businesses: Secondary victim (moderate/constrained) — must maintain multiple subscriptions; face cumulative cost burden and switching costs
 *   - Software Vendors: Primary beneficiary (institutional/arbitrage) — capture recurring revenue, reduced piracy losses, predictable future cash flows, ability to enforce price increases
 *   - Platform Operators: Secondary beneficiary (institutional/arbitrage) — cloud infrastructure providers (AWS, Azure, Google Cloud) extract rent from API lock-in and switching costs
 *   - Enterprise IT Departments: Mixed experience (organized/constrained) — benefit from centralized management and automatic updates; constrained by vendor contracts and lock-in
 *   - Open Source Coalition: Organized alternative (organized/mobile) — building escape routes (Linux, LibreOffice, Blender, open-source databases) with genuine mobility exit options
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing subscription model as inevitable outcome of digital economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subscription_economy_model, 0.52).
domain_priors:suppression_score(subscription_economy_model, 0.68).
domain_priors:theater_ratio(subscription_economy_model, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subscription_economy_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(subscription_economy_model, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(subscription_economy_model, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subscription_economy_model, tangled_rope).
narrative_ontology:human_readable(subscription_economy_model, "The Subscription Economy Model").
narrative_ontology:topic_domain(subscription_economy_model, "economic/technological").

domain_priors:requires_active_enforcement(subscription_economy_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subscription_economy_model, software_vendors).
narrative_ontology:constraint_beneficiary(subscription_economy_model, platform_operators).
narrative_ontology:constraint_beneficiary(subscription_economy_model, service_companies).
narrative_ontology:constraint_victim(subscription_economy_model, individual_consumers).
narrative_ontology:constraint_victim(subscription_economy_model, small_businesses).
narrative_ontology:constraint_victim(subscription_economy_model, consumer_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCKED-IN CONSUMER (SNARE) — Trapped in subscription dependencies. Critical software (Office 365, Creative Suite, productivity tools) is unavailable as perpetual licenses. Exit requires abandoning essential tools or switching to inferior alternatives. No viable path out without major disruption. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(subscription_economy_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (SNARE) — Must subscribe to multiple tools (accounting, design, collaboration, security). Exit is constrained by switching costs, retraining time, and compatibility with vendor ecosystems. Cumulative subscription costs erode profit margins. Cannot negotiate prices or exit terms individually. d≈0.80, f(d)≈1.20, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(subscription_economy_model, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ENTERPRISE IT DEPARTMENT (TANGLED ROPE) — Large organizations benefit from subscription model through automatic updates, centralized management, and predictable budgets. Also constrained by vendor lock-in, contract terms, and the difficulty of standardizing on open alternatives across large teams. Coordination function (centralized updates) exists but extraction is significant (no perpetual license discount, price escalation clauses). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: SOFTWARE VENDOR (ROPE) — Pure coordination of software delivery, updates, and support through subscription mechanism. Solves the vendor's problem of continuous funding for development. Benefits from predictable recurring revenue and reduced piracy. Experiences subscription model as coordination function with minimal coercive overhead. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(subscription_economy_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATOR (TANGLED ROPE) — Cloud providers (AWS, Azure, Google Cloud) coordinate infrastructure allocation while extracting rent from dependency. Customers benefit from elastic scaling and managed services. Customers are also locked into proprietary APIs, pricing opacity, and switching costs. Vendor benefits from increasing switching costs over time. d≈0.15, f(d)≈0.02, σ=1.2 → χ≈0.01. Near-zero effective extraction because the platform operator experiences the constraint as a beneficial coordination mechanism from its own perspective.
constraint_indexing:constraint_classification(subscription_economy_model, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: OPEN SOURCE COALITION (SCAFFOLD) — Organized alternative (Linux, LibreOffice, Blender, GIMP, open-source databases) building escape routes from subscription lock-in. Sees subscription model as temporary institutional arrangement with a sunset as open-source software matures. Coalition has mobile exit options (migrate to open alternatives) and growing capability. d≈0.30, f(d)≈0.27, σ=1.2 → χ≈0.18. Low effective extraction because coalition has agency.
constraint_indexing:constraint_classification(subscription_economy_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: LEGACY SOFTWARE INDUSTRY (PITON) — Subscription shift is partly performative maintenance of a business model that worked when software was scarce and distribution expensive. Digital distribution and cloud infrastructure have eliminated original scarcity, but subscription model persists through vendor inertia and switching-cost engineering. Theater ratio reflects that much subscription overhead (licensing servers, usage tracking, account management) is theatrical — functionally redundant but enforced for extraction. d≈0.10, f(d)≈-0.08, σ=1.2 → χ≈-0.05.
constraint_indexing:constraint_classification(subscription_economy_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, recurring revenue optimization is an immutable feature of capitalist firms: software vendors naturally gravitate toward subscription because it maximizes lifetime customer value and reduces piracy losses. Subscription is viewed as a natural law of digital economics, not a contingent institutional arrangement. However, structural data (ε=0.52, suppression=0.68, theater=0.55) contradicts the mountain classification — the engine will flag this as a false summit.
constraint_indexing:constraint_classification(subscription_economy_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subscription_economy_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subscription_economy_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subscription_economy_model, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subscription_economy_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(subscription_economy_model, TR),
    TR >= 0.70.

:- end_tests(subscription_economy_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Vendors capture significant value through recurring revenue, price escalation, and switching-cost engineering. The constraint is not maximal extraction (0.70+) because legitimate coordination benefits exist (cloud infrastructure, continuous updates, automatic security patches) and significant open-source alternatives exist at lower extraction rates. The rising trajectory (0.15→0.52 over 10 years) reflects vendor decisions to eliminate perpetual license alternatives and engineer deeper lock-in. Suppression (0.68): High. Significant barriers to exit include: (1) switching costs in learning new tools, (2) file format lock-in (proprietary APIs, cloud-native data formats), (3) network effects (teams standardized on specific tools), (4) vendor elimination of perpetual license alternatives as pricing pressure, (5) web-based design that requires subscriptions to access features. Theater ratio (0.55): Moderate. Subscription delivery mechanisms include both functional components (automatic updates, cloud infrastructure) and theatrical components (licensing servers, usage tracking, account management verification). The theater has increased as vendors added more monitoring and enforcement infrastructure, but it has not reached piton levels (0.70+) because genuine functional coordination still exists.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a sharp perspectival divide between beneficiaries and victims. Vendors and platform operators see pure coordination (Rope) — solving the legitimate problem of continuous software delivery. Consumers see pure extraction (Snare) — locked in without exit options. Open-source coalition sees a temporary institutional arrangement with a clear sunset (Scaffold) — open-source alternatives are building real escape routes. Enterprise IT sees mixed coordination and extraction (Tangled Rope) — the model enables centralized management but constrains budget flexibility and long-term planning. The analytical observer risks naturalizing subscription as inevitable, but the structural data (rising extractiveness, rising theater, availability of open-source alternatives) reveals it as a contingent institutional arrangement, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Consumers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No viable exit options once software becomes essential. Small businesses: Victim + constrained → d≈0.80, f(d)≈1.20. High extraction but not maximal; can migrate to open-source at significant switching cost. Software vendors: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiaries; experience constraint as coordination mechanism. Platform operators: Beneficiary + arbitrage (but with partial victim status from open-source competition) → d≈0.15, f(d)≈0.02. Near-neutral because platform operators face genuine threat from open-source alternatives and bare metal deployments. Open-source coalition: Organized + mobile → d≈0.30, f(d)≈0.27. Low effective extraction; coalition has agency and visible exit paths. Enterprise IT: Mixed + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; benefit from coordination but constrained by vendor lock-in. Analytical observer: analytical → d≈0.72, f(d)≈1.15. False summit; observer risks naturalizing contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED VIA PERSPECTIVAL ANALYSIS: The subscription model is genuinely Tangled Rope — it combines real coordination benefits (automatic updates, cloud scalability, feature development funding) with severe asymmetric extraction (lock-in, price escalation, vendor monopoly). The mandatrophy is resolved by acknowledging that beneficiaries (vendors, platform operators) see Rope; victims (locked-in consumers, small businesses) see Snare; organized agents (open-source coalition) see Scaffold; institutional implementers (enterprise IT) see Tangled Rope. The constraint's true type is determined by the structural relationship (extraction + coordination + enforcement), not by the observer's experience. The false summit (mountain perspective naturalizing subscription as inevitable) is detected by the engine: ε=0.52 and suppression=0.68 violate mountain constraints (ε≤0.25, suppression≤0.05). The open-source scaffold is real — Linux, LibreOffice, Blender, GIMP represent genuine alternative pathways. The sunset is plausible as open-source software approaches feature parity in productivity, design, and infrastructure domains. The constraint's evolution from legitimate coordination mechanism (original software subscription models in 1990s) to extraction mechanism (current cloud subscription lock-in) is captured by the rising extractiveness trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    software_industry_alternatives,
    'Are open-source and free-tier alternatives at parity with commercial subscription software in functionality, ease of use, and feature completeness?',
    'Comparative feature audits, user switching costs, adoption rate tracking for open-source alternatives, enterprise deployment case studies',
    'If parity exists: scaffold perspective confirmed — open-source sunset is real. If significant gaps remain: lock-in is structural, not organizational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(software_industry_alternatives, empirical, 'Functional parity between open-source alternatives and subscription software').

omega_variable(
    consumer_switching_threshold,
    'What cumulative subscription cost threshold causes individuals and small businesses to shift to open-source or abandon entire software categories?',
    'Household budget data, small business accounting, adoption curves for open-source alternatives correlated with subscription cost increases, price elasticity studies',
    'If threshold is low (< $50/month): mass exodus to open alternatives is imminent. If high (> $200/month): current pricing may not trigger switching.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_switching_threshold, empirical, 'Cost threshold triggering consumer switching to alternatives').

omega_variable(
    regulatory_intervention_likelihood,
    'Will antitrust or consumer protection regulation force unbundling of subscription services or restore perpetual license availability?',
    'EU Digital Markets Act enforcement, FTC antitrust actions, legislative proposals for software licensing rights, patent reform outcomes',
    'If regulation mandates unbundling: extraction mechanism is constrained externally. If no regulation: market dynamics determine parity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_likelihood, conceptual, 'Likelihood of regulatory intervention on subscription practices').

omega_variable(
    vendor_sustainability_contradiction,
    'Are subscription revenue models sustainable if software becomes increasingly commodified and open-source options reach feature parity?',
    'Long-term financial analysis of vendor margins, market share trends for proprietary vs open-source, venture funding cycles for subscription startups',
    'If unsustainable: scaffold sunset is inevitable. If sustainable: subscription model is self-reinforcing through network effects.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(vendor_sustainability_contradiction, empirical, 'Sustainability of subscription revenue models under commodification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subscription_economy_model, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subeco_tr_t0, subscription_economy_model, theater_ratio, 0, 0.25).
narrative_ontology:measurement(subeco_tr_t5, subscription_economy_model, theater_ratio, 5, 0.4).
narrative_ontology:measurement(subeco_tr_t10, subscription_economy_model, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(subeco_be_t0, subscription_economy_model, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(subeco_be_t5, subscription_economy_model, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(subeco_be_t10, subscription_economy_model, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subscription_economy_model, resource_allocation).
narrative_ontology:affects_constraint(subscription_economy_model, software_vendor_lock_in).
narrative_ontology:affects_constraint(subscription_economy_model, cloud_infrastructure_monopoly).
narrative_ontology:affects_constraint(subscription_economy_model, digital_ownership_erosion).

% DUAL FORMULATION NOTE:
% The subscription economy model is upstream of specific vendor lock-in constraints. Software vendor lock-in (proprietary APIs, file formats, account dependency) is a downstream implementation of the broader subscription model. Cloud infrastructure monopoly (AWS/Azure/Google lock-in) creates additional extraction layers on top of software subscriptions. Digital ownership erosion (loss of perpetual license culture) is both cause and consequence. These stories form a constraint family where subscription model establishes the coordination/extraction framework, and vendor-specific lock-in mechanisms operationalize it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(subscription_economy_model, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
