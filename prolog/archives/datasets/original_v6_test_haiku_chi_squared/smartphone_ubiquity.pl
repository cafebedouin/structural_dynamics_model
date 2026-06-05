% ============================================================================
% CONSTRAINT STORY: smartphone_ubiquity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_smartphone_ubiquity, []).

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
 *   constraint_id: smartphone_ubiquity
 *   human_readable: The Smartphone Ubiquity Constraint
 *   domain: technological/social/economic
 *
 * SUMMARY:
 *   The smartphone has evolved from a communication device into a 'place
 *   within which we live' — a portable digital home that bridges physical and
 *   digital realities. This constraint exhibits Tangled Rope dynamics at its
 *   core: genuine coordination function (enabling communication, information
 *   access, and service delivery at global scale) coupled with substantial
 *   extraction (attention mining, behavioral modeling, surveillance,
 *   lock-in). The constraint's theater ratio (0.62) reflects the performative
 *   layer: smartphone manufacturers and platform operators maintain
 *   theatrical commitments to user privacy, autonomy, and choice while
 *   simultaneously maximizing attention capture and lock-in through dark
 *   patterns and algorithmic design. The constraint has intensified over 15
 *   years as essential services (banking, healthcare, employment, government,
 *   civic participation) have migrated exclusively to smartphone-mediated
 *   platforms, raising suppression from 0.42 to 0.68. This migration created
 *   a structural inversion: the smartphone evolved from a consumer choice to
 *   an infrastructural necessity. The constraint's extractiveness has tripled
 *   from 0.22 to 0.58 as platform operators layered attention extraction,
 *   behavioral prediction, and ecosystem lock-in on top of the coordination
 *   function. The constraint's future hinges on whether interoperability
 *   mandates (EU DMA, GDPR, proposed Digital Regulation Acts) can decouple
 *   coordination from extraction, or whether the coupling has become
 *   structurally irreversible.
 *
 * KEY AGENTS:
 *   - Unconnected Populations: Victims (powerless/trapped) — facing digital exclusion from essential services, no exit option
 *   - Attention Commons: Victim (powerless/trapped) — epistemic and behavioral commons degraded by platform-driven attention extraction
 *   - Individual Users: Mixed (moderate/constrained) — benefit from coordination, bear cost of attention extraction and lock-in
 *   - Platform Operators (Apple, Google, Meta, Amazon): Beneficiaries (institutional/arbitrage) — capture coordination benefits plus extraction surplus
 *   - Device Manufacturers: Secondary beneficiaries (institutional/arbitrage) — hardware lock-in and ecosystem dependency
 *   - Digital Rights Coalitions: Organized agents (organized/constrained) — advocating for interoperability, data portability, algorithmic transparency
 *   - Regulatory Bodies (EU, national DSAs): Organized agents (organized/constrained) — mandating structural reforms through DMA, GDPR, upcoming Digital Acts
 *   - Legacy Communication Infrastructure: Degraded beneficiaries (institutional/arbitrage) — SMS, email, telephone networks persist through performative compliance
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent choices as immutable laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(smartphone_ubiquity, 0.58).
domain_priors:suppression_score(smartphone_ubiquity, 0.68).
domain_priors:theater_ratio(smartphone_ubiquity, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(smartphone_ubiquity, extractiveness, 0.58).
narrative_ontology:constraint_metric(smartphone_ubiquity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(smartphone_ubiquity, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(smartphone_ubiquity, tangled_rope).
narrative_ontology:human_readable(smartphone_ubiquity, "The Smartphone Ubiquity Constraint").
narrative_ontology:topic_domain(smartphone_ubiquity, "technological/social/economic").

domain_priors:requires_active_enforcement(smartphone_ubiquity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, platform_operators).
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, digital_service_providers).
narrative_ontology:constraint_beneficiary(smartphone_ubiquity, convenience_seekers).
narrative_ontology:constraint_victim(smartphone_ubiquity, attention_commons).
narrative_ontology:constraint_victim(smartphone_ubiquity, unconnected_populations).
narrative_ontology:constraint_victim(smartphone_ubiquity, cognitive_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCONNECTED POPULATIONS (SNARE) — Digital exclusion from essential services. Those without smartphones face increasing barriers to employment, healthcare, banking, and civic participation. No exit option: social infrastructure has migrated to digital platforms. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.98. Pure extraction with high suppression.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ATTENTION COMMONS (SNARE) — Epistemic and social commons degraded by extraction of attention and behavioral data. Users experience compulsive engagement by design (dark patterns, algorithmic feeds). Cannot exit without losing access to essential digital services. d≈0.93, f(d)≈1.40, σ=1.2 → χ≈0.95. High coercion, minimal consent.
constraint_indexing:constraint_classification(smartphone_ubiquity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDIVIDUAL USER (TANGLED ROPE) — Experiences dual function: genuine coordination (communication, access to information, community) and extraction (attention mining, behavioral modeling, filter bubbles). Constrained exit: smartphone is now primary access point to essential services. d≈0.68, f(d)≈1.08, σ=1.2 → χ≈0.75. Mixed coordination and high extraction.
constraint_indexing:constraint_classification(smartphone_ubiquity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: DIGITAL RIGHTS COALITIONS (TANGLED ROPE) — Organized agents (EFF, Mozilla, interoperability advocates, DMA regulators in EU) recognize the constraint provides coordination benefits (portability, accessibility, connectivity) AND extraction (platform lock-in, surveillance capitalism, algorithmic control). See structured pathway toward sunset: interoperability mandates, app store competition, data portability rights. d≈0.42, f(d)≈0.42, σ=1.1 → χ≈0.27. Low effective extraction due to organized agency and visible exit path.
constraint_indexing:constraint_classification(smartphone_ubiquity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATORS (ROPE) — Institutional beneficiaries (Apple, Google, Meta, etc.) experience the constraint as pure coordination: smartphone ubiquity solves the fundamental problem of connecting people to digital services at scale. The extraction (lock-in, data harvesting, attention capture) is their own innovation layered on top of the coordination function. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Negative effective extraction = net beneficiary. Can arbitrage: pivoting to new platforms, migrating ecosystems.
constraint_indexing:constraint_classification(smartphone_ubiquity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY BODIES (SCAFFOLD) — EU DMA, GDPR, DFS regulations, upcoming Digital Regulation Acts treat smartphone ubiquity as a temporary coordination failure with structured sunset: enforced interoperability, app store competition, algorithmic transparency, data portability. See the constraint as scaffolding that will be replaced by decentralized/federated alternatives (Signal, Matrix, ActivityPub ecosystem). d≈0.35, f(d)≈0.35, σ=1.1 → χ≈0.22. Declining enforcement as alternatives mature.
constraint_indexing:constraint_classification(smartphone_ubiquity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: LEGACY COMMUNICATION SYSTEMS (PITON) — Telephone networks, SMS, email, and previous-generation web standards persist as degraded alternatives. Theater ratio = 0.62: smartphone ubiquity maintains these systems through performative compliance (SMS fallback for 2FA, email for account recovery) rather than functional necessity. Institutional inertia keeps legacy infrastructure funded. d≈0.15, f(d)≈0.05, σ=0.9 → χ≈0.03. Minimal extraction; mostly theatrical persistence.
constraint_indexing:constraint_classification(smartphone_ubiquity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, smartphone ubiquity could be seen as reflecting immutable constraints: communication complexity requires portable devices; global connectivity requires centralized infrastructure; behavioral modeling is inherent to algorithmic optimization. HOWEVER: base extraction ε=0.58 and suppression=0.68 contradict the mountain gate (ε ≤ 0.25, suppression ≤ 0.05). This perspective registers as a FALSE SUMMIT — the framing of 'necessity' naturalizes what are contingent economic and political choices (monopoly structure, surveillance business models, platform lock-in).
constraint_indexing:constraint_classification(smartphone_ubiquity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(smartphone_ubiquity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(smartphone_ubiquity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(smartphone_ubiquity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(smartphone_ubiquity, TR),
    TR >= 0.70.

:- end_tests(smartphone_ubiquity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.58): Moderate-high. The smartphone enables genuine coordination — reducing communication costs, providing information access, enabling service delivery — but this function has been systematically coupled with extraction mechanisms: attention capture through dark patterns, behavioral prediction for targeting, ecosystem lock-in preventing switching, data harvesting for secondary markets. The extraction is not accidental; it is the primary value capture mechanism for platform operators. The rise from 0.22 to 0.58 reflects the intensification of this coupling as essential services migrated to platform-mediated access. Suppression (σ=0.68): High. Significant barriers to exit include: (1) Essential service lock-in: healthcare, banking, government, employment now require smartphone access; (2) Network effects: social value concentrates on dominant platforms, making alternatives unviable; (3) Technical barriers: non-interoperable ecosystems and proprietary APIs prevent switching; (4) Behavioral lock-in: dark patterns and algorithmic feeds create compulsive engagement. However, suppression is not total (approaching 1.0) because some individuals and populations maintain partial alternatives, and regulatory intervention is beginning to create exit pathways. Theater ratio (θ=0.62): Moderate-high. Smartphone and platform ecosystems maintain performative commitments to user privacy, autonomy, and choice that are substantially theatrical. Examples: (a) Privacy theater — platform privacy policies exist but create illusion of control while extensive behavioral tracking persists; (b) Consent theater — users are asked to 'consent' to terms of service that are incomprehensible and unavoidable; (c) Competition theater — app stores present choice while maintaining walled-garden constraints; (d) Regulation theater — platforms adopt compliance departments while optimizing extraction around regulatory gaps. The theater has intensified from 0.28 to 0.62 as platforms have become more sophisticated at maintaining legitimacy while maximizing extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates polarization across power structures. Unconnected and powerless agents (perspectives 1-2) see pure extraction (Snare) — essential services are inaccessible without smartphones, and the smartphone itself is an extraction device. Individual users (perspective 3) see mixed coordination and extraction (Tangled Rope) — they benefit from communication and access but at cost of attention mining and behavioral modeling. Organized agents (perspective 4) see a hybrid with a visible sunset (Tangled Rope moving toward Scaffold) — regulatory mandates and interoperability coalitions are building exit pathways. Platform operators (perspective 5) see pure coordination (Rope) — they genuinely solved the problem of connecting people to digital services at scale, and the extraction mechanisms are their legitimate business model. Legacy systems (perspective 7) see degradation (Piton) — they persist through performative compliance rather than functional necessity. The analytical observer (perspective 8) risks naturalizing the entire constraint as immutable law, obscuring the fact that smartphone ubiquity's extraction mechanisms are contingent design choices, not physical laws. The perspectival gap reveals that the constraint's classification depends entirely on structural position: beneficiaries see coordination; victims see extraction; organized agents see a problem being solved; regulators see a problem requiring intervention.
 *
 * DIRECTIONALITY LOGIC:
 *   Unconnected populations: Victim + trapped → d≈0.95, f(d)≈1.42. No exit means maximum directionality toward victimhood. Attention commons: Victim + trapped → d≈0.93, f(d)≈1.40. Abstract collective, cannot organize or exit. Individual users: Victim + constrained → d≈0.68, f(d)≈1.08. Can exit but at significant cost (loss of essential services, social isolation). Digital rights coalitions: Organized + constrained → d≈0.42, f(d)≈0.42. Have agency and visible exit pathways, but currently constrained by platform dominance. Platform operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Can exit or pivot to new platforms; have full control over their own constraints. Regulatory bodies: Organized + constrained → d≈0.38, f(d)≈0.37. Have institutional power but constrained by global platform scale and political will. Legacy systems: Beneficiary + arbitrage → d≈0.12, f(d)≈0.02. Minimal extraction because their role has degraded to theatrical persistence. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Risks high directionality toward victimhood through mislabeling contingent arrangements as natural laws.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing how indexical classification reveals the structural distribution of power. The mandatrophy question is: 'Is smartphone ubiquity a coordination solution or an extraction mechanism?' The answer: both, but whose perspective dominates determines the answer. For powerless agents without exit options, it is pure extraction (Snare). For platform operators with full arbitrage, it is pure coordination (Rope). For individuals with constrained choices, it is mixed (Tangled Rope). For organized coalitions with visible exit pathways, it is temporary extraction being solved (Scaffold). The mandatrophy is resolved by recognizing that all six classifications are structurally accurate from their respective observation points. The constraint is not mislabeled; rather, it is a presheaf of classifications indexed by power structure, temporal horizon, exit capacity, and scope. The false summit (analytical mountain perspective) is caught by the base metrics: ε=0.58 and suppression=0.68 violate the mountain thresholds (ε ≤ 0.25, suppression ≤ 0.05). The analytical observer's naturalization is revealed as an exercise of power — treating contingent arrangements as inevitable. Mandatrophy fully resolved by the perspectival presheaf.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_option_availability,
    'Can meaningful digital participation exist outside of smartphone-mediated platforms, or has the coordination function become genuinely inseparable from the extraction mechanism?',
    'Empirical analysis of alternative digital ecosystems: adoption rates of non-smartphone federated services (Matrix, Mastodon, ActivityPub), success metrics of phone-free digital participation, viability of alternative hardware/OS ecosystems (Linux phones, dumb phones with essential services)',
    'If alternatives viable: constraint is Tangled Rope with real sunset (extraction can be decoupled from coordination). If alternatives fail: constraint approaches Snare (extraction becomes inseparable from essential access).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_option_availability, empirical, 'Whether meaningful digital participation can occur outside smartphone platforms').

omega_variable(
    interoperability_sufficiency,
    'Does enforced interoperability (app store competition, data portability, algorithm transparency) actually reduce extraction, or do platform operators extract through higher-level coupling mechanisms?',
    'Comparative analysis of pre- and post-interoperability regulation periods; measurement of attention capture, behavioral prediction accuracy, and lock-in strength under interoperability regimes; case studies from EU DMA enforcement',
    'If sufficient: scaffold perspective confirmed — regulatory sunset is achieving decoupling. If insufficient: extraction persists through network effects and behavioral design regardless of technical interoperability.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_sufficiency, empirical, 'Whether interoperability mandates effectively reduce platform extraction').

omega_variable(
    essential_service_digitization_irreversibility,
    'Is the migration of essential services (healthcare, banking, government, employment) to smartphone platforms structurally reversible, or has a point of no return been crossed?',
    'Historical analysis of service migration decisions; institutional barriers to maintaining non-digital alternatives; cost analysis of dual-system maintenance; political will assessment for service de-digitization in major economies',
    'If reversible: suppression can be structurally reduced (alternatives can re-emerge). If irreversible: suppression is now path-dependent constraint on societies (no exit means constraint approaches Mountain despite high ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(essential_service_digitization_irreversibility, empirical, 'Whether service digitization is structurally reversible').

omega_variable(
    attention_economy_fundamentality,
    'Is attention extraction (behavioral modeling, algorithmic engagement) fundamental to smartphone functionality, or is it a contingent business model choice?',
    'Comparison of non-surveillance smartphone ecosystems (iPhone privacy features, Fairphone, degoogled Android); analysis of whether attention extraction is technically necessary for coordination function or economically necessary only for advertising-based revenue',
    'If contingent: extraction can be decoupled through alternative business models (subscription, hardware sales, public funding). If fundamental: extraction is inherent to smartphone ubiquity — constraint approaches snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attention_economy_fundamentality, conceptual, 'Whether attention extraction is inherent to smartphone functionality or contingent to business models').

omega_variable(
    global_connectivity_prerequisite,
    'Does global smartphone ubiquity require centralized platform operators, or can decentralized infrastructure achieve equivalent connectivity?',
    'Technical feasibility studies of mesh networks, satellite internet, municipal broadband; economic analysis of decentralized infrastructure scaling; case studies of functioning alternative networks (community-owned broadband, decentralized social networks)',
    'If decentralization feasible: platform operators are contingent beneficiaries (not necessary for coordination). If decentralization fails: centralization is a structural necessity (constraint approaches Mountain in that dimension).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_connectivity_prerequisite, empirical, 'Whether decentralized infrastructure can provide equivalent global connectivity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(smartphone_ubiquity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(smub_tr_t0, smartphone_ubiquity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(smub_tr_t7, smartphone_ubiquity, theater_ratio, 7, 0.45).
narrative_ontology:measurement(smub_tr_t15, smartphone_ubiquity, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(smub_be_t0, smartphone_ubiquity, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(smub_be_t7, smartphone_ubiquity, base_extractiveness, 7, 0.42).
narrative_ontology:measurement(smub_be_t15, smartphone_ubiquity, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(smartphone_ubiquity, global_infrastructure).
narrative_ontology:affects_constraint(smartphone_ubiquity, digital_surveillance_capitalism).
narrative_ontology:affects_constraint(smartphone_ubiquity, platform_lock_in_extraction).
narrative_ontology:affects_constraint(smartphone_ubiquity, attention_economy_commons_degradation).
narrative_ontology:affects_constraint(smartphone_ubiquity, digital_divide_access_inequality).

% DUAL FORMULATION NOTE:
% Smartphone ubiquity is a constraint family with multiple structurally distinct claims: (1) the coordination problem of global connectivity (ε≈0.15, Rope), (2) the attention extraction and behavioral modeling layer (ε≈0.72, Snare), (3) the lock-in and interoperability blocking (ε≈0.60, Tangled Rope), (4) the essential service dependency (ε≈0.68, Snare). This story focuses on the unified constraint at the ecosystem level (ε=0.58, Tangled Rope) and links to the sibling constraints that decompose the smartphone ubiquity into their structurally distinct mechanisms. The upstream constraint is global_infrastructure (coordination function, ε≈0.15); the downstream constraints are extraction mechanisms and lock-in that layer on top.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(smartphone_ubiquity, organized, 0.38).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
