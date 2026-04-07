% ============================================================================
% CONSTRAINT STORY: internet_evolution_lifecycle
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_internet_evolution_lifecycle, []).

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
 *   constraint_id: internet_evolution_lifecycle
 *   human_readable: The Lifecycle of the Open Internet
 *   domain: technological/social
 *
 * SUMMARY:
 *   The internet emerged as a decentralized coordination mechanism for
 *   information exchange (TCP/IP, DNS, HTTP) designed to route around
 *   centralized control. Over its lifecycle, it has transformed into a
 *   platform for centralized extraction of user attention, behavioral data,
 *   and communication metadata. This constraint models how the same
 *   technological infrastructure that enabled open communication now enables
 *   unprecedented concentration of intermediation power and extraction of
 *   economic value from user participation. The transition from coordination
 *   to extraction is not technologically determined but reflects specific
 *   policy choices, business models, and governance failures that allowed
 *   network effects to concentrate in the hands of a small number of platform
 *   operators.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/trapped) — participate in online communication and commerce but cannot exit platforms without losing connectivity to essential social and economic infrastructure
 *   - Small Content Creators: Primary victims (powerless/trapped) — depend on platforms for audience reach but have no control over algorithmic amplification, demonetization, or removal
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture advertising revenue, data value, and network effects; have multiple exit options including regulatory arbitrage and vertical integration
 *   - Advertising Networks: Secondary beneficiaries (institutional/arbitrage) — access to detailed user attention and behavioral data; can build detailed targeting profiles
 *   - Data Aggregators: Secondary beneficiaries (institutional/arbitrage) — acquire user data for resale to third parties; control pricing in data broker markets
 *   - Regulatory Authorities: Constrained institutional actors (moderate/constrained) — responsible for managing harms (illegal content, disinformation, market concentration) but lack technical capacity and cross-border jurisdiction to regulate effectively
 *   - Internet Governance Institutions: Degraded institutional actors (institutional/arbitrage) — ICANN, IETF, W3C maintain procedural neutrality but lack enforcement capacity against platform unilateral action
 *   - Analytical Observer: Observes from civilizational/universal perspective; risks naturalizing contingent network effect concentration as inevitable law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(internet_evolution_lifecycle, 0.58).
domain_priors:suppression_score(internet_evolution_lifecycle, 0.52).
domain_priors:theater_ratio(internet_evolution_lifecycle, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(internet_evolution_lifecycle, extractiveness, 0.58).
narrative_ontology:constraint_metric(internet_evolution_lifecycle, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(internet_evolution_lifecycle, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(internet_evolution_lifecycle, tangled_rope).
narrative_ontology:human_readable(internet_evolution_lifecycle, "The Lifecycle of the Open Internet").
narrative_ontology:topic_domain(internet_evolution_lifecycle, "technological/social").

domain_priors:requires_active_enforcement(internet_evolution_lifecycle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, platform_operators).
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, advertising_networks).
narrative_ontology:constraint_beneficiary(internet_evolution_lifecycle, data_aggregators).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, end_users).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, small_content_creators).
narrative_ontology:constraint_victim(internet_evolution_lifecycle, information_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: END USER (SNARE) — Trapped in platform ecosystems with no realistic exit. Surveillance, algorithmic manipulation, content filtering, and data extraction are non-negotiable terms of participation. Cannot walk away without losing access to essential communication infrastructure. Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL CONTENT CREATORS (SNARE) — Dependent on platforms for audience reach. Subject to algorithmic amplification control, demonetization, shadowbanning, and content removal at platform discretion. Cannot build independent audience infrastructure due to network effects. Extraction of labor value and attention is structural and inescapable.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AUTHORITY (TANGLED ROPE) — Constrained by complexity of cross-border governance and technical coordination requirements. Benefits from the internet as information infrastructure and economic driver, but also bears costs of managing illegal content, disinformation, and market concentration. Active enforcement required but limited exit options create asymmetric power dynamics with platform operators.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PLATFORM OPERATORS (ROPE) — Primary beneficiaries experiencing the constraint as pure coordination problem: aggregating user attention, matching advertisers, managing content at scale. Network effects create positive feedback. Exit options include regulatory arbitrage between jurisdictions and vertical integration. Extraction runs toward this agent, not away.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNET GOVERNANCE LEGACY (PITON) — Decentralized protocols (DNS, TCP/IP, HTTP) and open standards were design principles from the internet's origin, but their governance is increasingly theater. ICANN, IETF, and W3C maintain procedural neutrality while actual power concentrates in platform operators who can unilaterally change protocols through implementation. The open internet mythology persists, but the functional verification of 'openness' is degraded. Theater ratio high due to performative governance maintained through institutional inertia.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some concentration of intermediation power is inherent to network effects: a communication medium's utility is proportional to the number of participants, and organizing mass participation requires centralized coordination. This perspective sees platform concentration as an unavoidable property of how network-based communication at scale must function. However, the structural data contradicts mountain classification — the engine will compute this as a false summit, revealing that network effect concentration is a contingent economic arrangement, not a law of nature.
constraint_indexing:constraint_classification(internet_evolution_lifecycle, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(internet_evolution_lifecycle_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(internet_evolution_lifecycle, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(internet_evolution_lifecycle, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(internet_evolution_lifecycle, TR),
    TR >= 0.70.

:- end_tests(internet_evolution_lifecycle_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platform operators extract substantial economic value from user data and attention, but the extraction is not as severe as a pure monopoly would impose because some alternative platforms exist and switching costs, while high, are not absolute. The measurement trajectory (0.15 → 0.58 over 20 years) reflects the historical transition from decentralized coordination to platform concentration. Suppression (0.52): Moderate. Network effects create significant barriers to independent platform creation, but suppression is not total — decentralized and federated alternatives exist, though with reduced network value. User switching costs are high but not impossible. Regulation creates some constraint on platform behavior, though enforcement is weak. Theater ratio (0.65): Moderate-high and increasing. Open internet governance through ICANN, IETF, and W3C maintains procedural legitimacy, but decision-making power in practice concentrates in platform operators who can unilaterally change protocols through implementation choices. The mythology of the open internet persists while actual openness declines — theater increases as the gap between procedural and substantive governance widens.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a dramatic perspectival gap driven by exit options and beneficiary status. Platform operators see pure coordination (Rope) — the internet is a tool for connecting users with content and advertisers with targeting opportunities. Regulatory authorities see mixed coordination and extraction (Tangled Rope) — they benefit from the internet's economic productivity but bear costs of managing illegal content and market concentration. End users see pure extraction (Snare) — participation requires accepting surveillance, algorithmic manipulation, and data extraction with no meaningful exit option. The open internet governance institutions see their own degraded role (Piton) — they maintain procedural authority through tradition (IETF RFCs, ICANN consensus) but lack enforcement capacity against platform unilateral action. The civilizational analytical observer risks seeing network effect concentration as a natural law (Mountain), when the structural data reveals it as contingent on specific business models and governance choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural position in the extraction flow. End users: beneficiary of communication infrastructure, but victim of extraction — d high (0.85-0.95), experiencing maximum chi. Platform operators: beneficiary of network effects and data monopoly — d low (0.05-0.15), experiencing negative or minimal chi. Regulatory authorities: constrained by technical complexity and cross-border jurisdiction — d moderate (0.55-0.65). The piton classification reflects high theater (performative governance) despite institutional power, deriving from the gap between ICANN/IETF procedural authority and actual protocol control in platform operators' hands. The mountain classification at the analytical perspective risks naturalizing network effect concentration as immutable; the engine's false summit detector identifies this as contingent institutional arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy for this constraint lies in the distinction between platform-optimized narratives of open coordination and structural evidence of extractive consolidation. The platform operator perspective (Rope) claims the internet is a pure coordination mechanism that benefits all participants through connection and information access. The end user perspective (Snare) documents systematic extraction: surveillance capitalism, algorithmic filter bubbles, content removal without transparency, data theft, manipulation of attention. The regulatory authority perspective (Tangled Rope) suggests both are partially true — real coordination benefits exist alongside real extraction mechanisms. The mandatrophy resolves by recognizing that the same technical infrastructure can function as both coordination and extraction depending on the governance layer. The internet's protocols are genuinely open (TCP/IP, HTTP are public standards). But the governance layer — business models, data ownership, algorithmic control, content moderation rules — is proprietary and extractive. The constraint is not 'is the internet open or closed?' but 'who controls the governance layer?' The open internet mythology resolves the mandatrophy by allowing both narratives to coexist: the platform operator claims openness at the protocol layer while practicing control at the governance layer; the end user claims closedness at the governance layer while protocol openness is irrelevant to their exit options. The constraint type depends on which layer is primary — but the structural data suggests the governance layer (proprietary, extractive) is primary in determining user experience.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    openness_measurement_threshold,
    'What constitutes a meaningful technical or operational threshold for distinguishing an ''open'' internet architecture from a ''closed'' one?',
    'Historical comparison of interoperability metrics (protocol adoption, cross-platform data portability, decentralized alternative viability) with platform revenue concentration and user exit costs',
    'If threshold focuses on protocol neutrality: many centralized platforms remain ''open'' by definition. If threshold focuses on user exit costs and market concentration: platforms are operationally closed despite technical openness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(openness_measurement_threshold, conceptual, 'Threshold for operationally distinguishing open from closed internet').

omega_variable(
    network_effect_inevitability,
    'Are platform consolidation and data concentration inevitable consequences of network effects, or are they contingent outcomes of specific policy and design choices?',
    'Comparative case study of alternative architectures (federated systems like ActivityPub, decentralized identity systems, interoperable messaging protocols) measuring whether they achieve viability without concentration',
    'If inevitable: mountain perspective justified; concentration is immutable. If contingent: platform dominance is extractive institutional arrangement; constraint type shifts toward snare/tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether platform consolidation is inevitable or contingent').

omega_variable(
    user_agency_exit_barriers,
    'Are user exit barriers primarily technical (switching costs, data portability) or social (network effects, relationship lock-in, content history ownership)?',
    'User surveys on barriers to platform switching; technical analysis of data portability costs; measurement of alternate platform adoption rates when technical portability is available',
    'If primarily technical: policy solutions (interoperability mandates, data export rights) can reduce exit costs and constraint severity. If primarily social: technical solutions alone insufficient; requires governance changes or new coordination mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_agency_exit_barriers, empirical, 'Relative magnitude of technical vs social exit barriers').

omega_variable(
    decentralization_viability_timeline,
    'What is the realistic timeline for decentralized/federated internet architectures to achieve critical mass sufficient to compete with centralized platforms on user experience and content discovery?',
    'Historical adoption curves of decentralized platforms (Mastodon, Bluesky, Lemmy); comparative UX analysis with centralized platforms; measurement of ''cold start'' problem severity',
    'If timeline < 10 years: viable exit pathway exists; constraint may transition to scaffold. If timeline > 30 years or indefinite: platforms remain locked-in; snare classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralization_viability_timeline, empirical, 'Timeline for decentralized internet viability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(internet_evolution_lifecycle, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inet_tr_t0, internet_evolution_lifecycle, theater_ratio, 0, 0.25).
narrative_ontology:measurement(inet_tr_t10, internet_evolution_lifecycle, theater_ratio, 10, 0.45).
narrative_ontology:measurement(inet_tr_t20, internet_evolution_lifecycle, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(inet_be_t0, internet_evolution_lifecycle, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(inet_be_t10, internet_evolution_lifecycle, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(inet_be_t20, internet_evolution_lifecycle, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(internet_evolution_lifecycle, global_infrastructure).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, data_extraction_surveillance).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, platform_lock_in).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, algorithmic_content_control).
narrative_ontology:affects_constraint(internet_evolution_lifecycle, network_effects_concentration).

% DUAL FORMULATION NOTE:
% The internet lifecycle constraint is upstream of several domain-specific constraints: data extraction and surveillance depends on internet infrastructure; platform lock-in is specific to internet platforms; algorithmic content control uses internet distribution; network effects concentration is substrate-level infrastructure problem. Each downstream constraint has its own ε value reflecting domain-specific extraction mechanisms. The lifecycle constraint represents the infrastructure layer enabling all downstream extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(internet_evolution_lifecycle, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
