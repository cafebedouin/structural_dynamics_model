% ============================================================================
% CONSTRAINT STORY: information_asymmetry_certification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_information_asymmetry_certification, []).

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
 *   constraint_id: information_asymmetry_certification
 *   human_readable: Information Asymmetry Certification Systems
 *   domain: general/institutional_coordination
 *
 * SUMMARY:
 *   Information asymmetry certification systems serve the legitimate function
 *   of solving coordination problems: buyers need reliable signals about
 *   product quality; employers need reliable signals about job candidate
 *   competence; investors need reliable signals about asset risk. But
 *   certification systems are also extraction mechanisms — gatekeepers can
 *   restrict market access, inflate barrier costs, and maintain their
 *   monopoly on credibility assessment. This constraint exhibits the full
 *   taxonomy of DR types from different structural positions. The information
 *   seeker (powerless/trapped) experiences pure snare: must depend on
 *   gatekeepers and cannot verify independently. The competing information
 *   provider (moderate/constrained) experiences tangled rope: must use
 *   certification to compete, but the certification process favors
 *   incumbents. The certifying institution (institutional/arbitrage)
 *   experiences rope: solving the genuine coordination problem of information
 *   reliability while capturing monopoly benefits. The decentralized network
 *   (organized/mobile) experiences scaffold: building alternative pathways
 *   with a sunset trajectory. The legacy credential system
 *   (institutional/arbitrage) experiences piton: maintained through inertia
 *   despite eroding functional content. The analytical observer risks the
 *   false summit of mountain: seeing information asymmetry as a natural law
 *   when it is a contingent institutional arrangement. The extractiveness
 *   trend (0.32 → 0.54 over 20 years) reflects credential inflation and
 *   gatekeeping entrenchment; the theater ratio trend (0.48 → 0.68) reflects
 *   that certification has become increasingly performative as actual
 *   verification capacity has stagnated.
 *
 * KEY AGENTS:
 *   - Information Seekers: Primary victim (powerless/trapped) — depend on certification for access to reliable signals; cannot independently verify; bear cost of gatekeeping-induced barriers
 *   - Certifying Institutions: Primary beneficiary (institutional/arbitrage) — capture monopoly rents through credential scarcity, licensing fees, and gatekeeping; control market access through certification standards
 *   - Information Gatekeepers: Secondary beneficiary (institutional/arbitrage) — gatekeepers who use certification to restrict market access and extract from those seeking entry
 *   - Competing Information Providers: Secondary victim (moderate/constrained) — must obtain certification to credibly compete; face incumbent advantage and barrier inflation
 *   - Market Efficiency: Abstract victim (powerless/trapped) — collective good bearing cost of information asymmetry; no mechanism to organize or exit
 *   - Decentralized Information Networks: Organized alternative (organized/mobile) — blockchain, open-source, peer-to-peer systems building parallel verification with sunset logic
 *   - Legacy Credential System: Institutional inertia (institutional/arbitrage) — academic degrees, professional licenses persisting through sunk investment despite credential inflation
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(information_asymmetry_certification, 0.54).
domain_priors:suppression_score(information_asymmetry_certification, 0.62).
domain_priors:theater_ratio(information_asymmetry_certification, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(information_asymmetry_certification, extractiveness, 0.54).
narrative_ontology:constraint_metric(information_asymmetry_certification, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(information_asymmetry_certification, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(information_asymmetry_certification, tangled_rope).
narrative_ontology:human_readable(information_asymmetry_certification, "Information Asymmetry Certification Systems").
narrative_ontology:topic_domain(information_asymmetry_certification, "general/institutional_coordination").

domain_priors:requires_active_enforcement(information_asymmetry_certification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(information_asymmetry_certification, certifying_institutions).
narrative_ontology:constraint_beneficiary(information_asymmetry_certification, information_gatekeepers).
narrative_ontology:constraint_victim(information_asymmetry_certification, information_seekers).
narrative_ontology:constraint_victim(information_asymmetry_certification, market_efficiency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION SEEKER (SNARE) — Trapped in dependence on certifying institutions to access reliable information. Cannot independently verify claims; must trust the certification process or remain ignorant. Market efficiency (the collective good of price discovery and accurate resource allocation) is the powerless victim — bears full cost of certification failure with no mechanism to organize.
constraint_indexing:constraint_classification(information_asymmetry_certification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING INFORMATION PROVIDER (TANGLED ROPE) — Constrained by certification gatekeeping: must obtain certification to credibly compete, but the certification process itself creates asymmetric advantage for incumbents. Benefits from using the certification system to establish credibility but also victimized by its extraction mechanisms. Moderate agency but significant structural constraints.
constraint_indexing:constraint_classification(information_asymmetry_certification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CERTIFYING INSTITUTION (ROPE) — Experiences certification as pure coordination: solving the genuine problem of information reliability through standardized verification. Arbitrage exit option (can monetize certification through fees, credentials, market access control). Net beneficiary — extraction flows toward this agent through monopoly pricing and gatekeeping.
constraint_indexing:constraint_classification(information_asymmetry_certification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DECENTRALIZED INFORMATION NETWORK (SCAFFOLD) — Organized agents (distributed ledgers, open-source verification, peer-to-peer rating systems) are building alternative verification pathways with a clear sunset: blockchain-based proofs, cryptographic signatures, and decentralized reputation systems are creating parallel certification mechanisms that bypass traditional gatekeepers. Low effective extraction because alternatives exist with clear maturation path.
constraint_indexing:constraint_classification(information_asymmetry_certification, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY CREDENTIAL SYSTEM (PITON) — Academic degrees, professional licenses, and institutional affiliations persist as certification mechanisms largely through inertia. Their actual information content has eroded as credential inflation and prestige arbitrage have decoupled credentials from competence. The theater ratio (0.68) reflects that much certification activity is performative signaling rather than functional verification. Maintained because alternatives haven't fully replaced it and institutional investment is sunk.
constraint_indexing:constraint_classification(information_asymmetry_certification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, information asymmetry is an irreducible feature of knowledge: no agent can directly experience all states of the world; all must rely on third-party attestation. Some certification is inherent to any epistemic system. However, the degree of certification and the concentration of certifying power are contingent institutional facts, not laws of nature. This perspective risks naturalizing extractive gatekeeping as an unavoidable cost of knowledge.
constraint_indexing:constraint_classification(information_asymmetry_certification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(information_asymmetry_certification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(information_asymmetry_certification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(information_asymmetry_certification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(information_asymmetry_certification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(information_asymmetry_certification, TR),
    TR >= 0.70.

:- end_tests(information_asymmetry_certification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.54): Moderate-high. The certification system does solve a real coordination problem (information reliability), but extraction has grown over time through credential inflation, gatekeeping, and barrier entrenchment. The intermediate value reflects that the system is hybrid — genuine coordination with embedded extraction. Suppression (0.62): High. Significant barriers to bypassing certification include: legal requirements (licensing laws, regulatory mandates), market norms (employers expect certified credentials), network effects (switching costs to alternative systems), and reputation asymmetry (uncertified claims are discounted regardless of quality). These barriers are not total — decentralized alternatives exist — but they are substantial. Theater ratio (0.68): High. Much certification activity is performative: credential signaling that doesn't correlate with actual competence (grade inflation, prestige arbitrage, diploma mills), ritual verification that doesn't detect incompetence (board certifications with no clinical update requirements), and status maintenance that has nothing to do with information (alumni networks, prestige universities). The theater has increased over the interval as certification has become increasingly decoupled from actual verification.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. The beneficiary (certifying institution) sees pure rope — solving the genuine problem of information asymmetry through standardized verification. The powerless victim sees pure snare — trapped dependence on gatekeepers with no exit. The moderate competitor sees tangled rope — must use certification to compete but the system favors incumbents. The organized alternative sees scaffold — decentralized systems are building parallel pathways with clear sunset. The legacy system sees piton — maintained through inertia despite eroding function. The analytical observer risks mountain — seeing information asymmetry as a law of nature when it is a contingent institutional fact. The perspectival gap is not an error in classification but the core diagnostic: the constraint's real structure is revealed by the divergence of experiences across structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from each agent's structural relationship to the information asymmetry constraint. Certifying institutions are beneficiaries with arbitrage exit (low d, low χ) — they capture monopoly rents through gatekeeping. Information seekers are victims with trapped exit (high d, high χ) — they cannot independently verify and must depend on gatekeepers. Competing providers are victims with constrained exit (moderate-high d, moderate χ) — they can theoretically bypass certification (arbitrage exists) but face high practical costs through market and legal barriers. The decentralized network has organized power and mobile exit (low-moderate d, low χ) — the existence of alternative pathways means certification is no longer the sole verification mechanism. This differentiated directionality explains why the same certification system produces such different classifications from different positions: the constraint's extraction asymmetry is real and experienced differently depending on structural location.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID CERTIFICATION CASE: This constraint resolves the mandatrophy by distinguishing the certification function (genuine coordination problem: information reliability) from the certification mechanism (institutional gatekeeping: extraction). A pure rope system would solve information asymmetry with minimal coercion. A pure snare would extract without any coordination benefit. This system is tangled rope: it both solves the coordination problem AND extracts through monopoly gatekeeping. The mandatrophy resolution requires declaring both the beneficiary (coordination benefit to certifying institutions) and the victim (extraction cost to information seekers). The scaffold perspective (decentralized alternatives) shows that the tangled rope can transition to pure rope if the coordination function is provided through non-extractive mechanisms. The piton perspective (legacy credentials) shows that theatrical elements dominate in domains where actual verification has become impossible. The mountain perspective is a false summit: information asymmetry is not a natural law, but the institutional arrangements that concentrate certifying power have been naturalized as such.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    certifier_conflict_of_interest,
    'Can a certifying institution remain neutral when its economic incentives are tied to limiting market access or maintaining information asymmetry?',
    'Comparative analysis of certification failure rates under different business models: non-profit vs for-profit, monopoly vs competitive, vertically integrated vs independent',
    'If certifier conflict is inherent: certification systems are necessarily extractive (snare baseline). If addressable through structure: certification can be pure rope with appropriate governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certifier_conflict_of_interest, empirical, 'Whether certifier conflicts of interest are inherent or structural').

omega_variable(
    verification_cost_distribution,
    'Is the cost of certification barriers absorbed by those seeking information or by society through reduced market efficiency?',
    'Cost accounting across certification pathways: direct fees vs opportunity costs of gatekeeping-induced inefficiency; comparison of information quality improvements to resource spent on certification',
    'If mostly private cost to seekers: extractiveness < 0.40 (moderate snare). If societal cost is primary: extractiveness > 0.65 (severe snare with systemic damage).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_distribution, empirical, 'Distribution of certification barrier costs').

omega_variable(
    alternative_certification_maturation,
    'How long until decentralized, cryptographic, or peer-review-based certification systems mature enough to bypass traditional gatekeepers?',
    'Adoption curves for alternative systems; market share displacement; regulatory acceptance of non-institutional verification methods',
    'If < 5 years: scaffold perspective is dominant — sunset is imminent. If > 20 years: legacy system (piton) persists indefinitely. If never: mountain perspective is correct — certification bottleneck is unavoidable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_certification_maturation, empirical, 'Timeline for decentralized certification maturity').

omega_variable(
    information_complexity_ceiling,
    'Is there a complexity threshold beyond which certification systems necessarily fail to verify accuracy, creating incurable information asymmetry?',
    'Empirical study of certification failure rates as information complexity increases; identification of domains where verification becomes impossible within economic constraints',
    'If ceiling exists at moderate complexity: certification is structurally necessary (rope baseline). If no ceiling: current barriers are contingent (snare is accurate diagnosis).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_complexity_ceiling, empirical, 'Existence of verification complexity ceiling').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(information_asymmetry_certification, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iac_tr_t0, information_asymmetry_certification, theater_ratio, 0, 0.48).
narrative_ontology:measurement(iac_tr_t10, information_asymmetry_certification, theater_ratio, 10, 0.6).
narrative_ontology:measurement(iac_tr_t20, information_asymmetry_certification, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(iac_be_t0, information_asymmetry_certification, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(iac_be_t10, information_asymmetry_certification, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(iac_be_t20, information_asymmetry_certification, base_extractiveness, 20, 0.54).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(information_asymmetry_certification, information_standard).
narrative_ontology:affects_constraint(information_asymmetry_certification, credential_inflation).
narrative_ontology:affects_constraint(information_asymmetry_certification, market_access_gatekeeping).
narrative_ontology:affects_constraint(information_asymmetry_certification, knowledge_commons_tragedy).

% DUAL FORMULATION NOTE:
% Information asymmetry certification can be decomposed into multiple structurally distinct constraints: (1) the genuine coordination problem of signal reliability (low ε, rope baseline), (2) the extraction mechanism of credential gatekeeping (high ε, snare dynamics), and (3) the theatrical credential ritual (high theater, piton dynamics). These are separate stories sharing a family relationship — a single certification institution delivers all three. The unified story treats the institution as a tangled rope solving coordination while extracting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(information_asymmetry_certification, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
