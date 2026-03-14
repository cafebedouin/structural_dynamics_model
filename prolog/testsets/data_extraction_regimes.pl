% ============================================================================
% CONSTRAINT STORY: data_extraction_regimes
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_extraction_regimes, []).

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
 *   constraint_id: data_extraction_regimes
 *   human_readable: Data Extraction Regimes in Digital Platforms
 *   domain: digital_economy/information_asymmetry
 *
 * SUMMARY:
 *   Data extraction regimes constitute the structural mechanism by which
 *   digital platforms capture, process, and monetize information about user
 *   behavior, preferences, and identity. The constraint operates at multiple
 *   scales: individual users generate granular behavioral data; platforms
 *   aggregate and derive inferences at population scale; advertisers and data
 *   aggregators purchase access to derived insights; information commons are
 *   contaminated by surveillance asymmetries. The regime is enforced through
 *   terms of service (legal), technical architecture (code as law), and
 *   market concentration (economic). The extraction is masked by claims of
 *   inevitable technological necessity, user consent narratives, and
 *   free-service reciprocity. This constraint demonstrates why the same
 *   structural phenomenon classifies as snare from the user perspective
 *   (trapped, no real exit, extraction is invisible and non-negotiable), as
 *   rope from the platform perspective (coordination function is genuine:
 *   recommendation requires signals), as tangled rope from the regulatory
 *   perspective (genuine coordination attempt alongside continued
 *   extraction), and as scaffold from the alternative governance perspective
 *   (emerging exit pathways with sunset logic). The theater ratio (0.58,
 *   rising to 0.58 by interval end) reflects that data extraction is
 *   substantially masked: users see interface theater (privacy controls,
 *   opt-out buttons) that give appearance of choice while real extraction
 *   continues at infrastructure scale.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary victims (powerless/trapped) — mandatory extraction via service dependency; no viable exit without social/economic harm
 *   - Privacy-Conscious Users: Secondary victims (moderate/constrained) — can employ privacy tools and exit at high personal cost; constrained rather than fully trapped
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — extract massive value from user data signals; experience regime as coordination mechanism
 *   - Advertising Networks: Secondary beneficiaries (institutional/arbitrage) — purchase derived data products; depend on platform extraction
 *   - Regulatory Regimes: Organized actors (organized/mobile) — attempt to impose coordination constraints (GDPR, FTC enforcement) with partial success; navigate regulatory arbitrage by platforms
 *   - Alternative Data Governance: Organized coalitions (organized/constrained) — building cooperative data trusts, federated platforms, personal data pods; seeing decentralization as sunset pathway
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent architectural choices as inevitable technical requirements
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_extraction_regimes, 0.68).
domain_priors:suppression_score(data_extraction_regimes, 0.72).
domain_priors:theater_ratio(data_extraction_regimes, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_extraction_regimes, extractiveness, 0.68).
narrative_ontology:constraint_metric(data_extraction_regimes, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(data_extraction_regimes, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_extraction_regimes, snare).
narrative_ontology:human_readable(data_extraction_regimes, "Data Extraction Regimes in Digital Platforms").
narrative_ontology:topic_domain(data_extraction_regimes, "digital_economy/information_asymmetry").

domain_priors:requires_active_enforcement(data_extraction_regimes).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_extraction_regimes, platform_operators).
narrative_ontology:constraint_beneficiary(data_extraction_regimes, data_aggregators).
narrative_ontology:constraint_beneficiary(data_extraction_regimes, advertising_networks).
narrative_ontology:constraint_victim(data_extraction_regimes, data_subjects).
narrative_ontology:constraint_victim(data_extraction_regimes, information_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Trapped in mandatory data extraction by account creation and service dependency. No viable exit without abandoning social/economic participation. Suppression is structural and internalized: terms of service are incomprehensible by design, data flows are opaque, and surveillance is invisible. Maximum experienced extraction because the subject cannot articulate, negotiate, or escape the constraint.
constraint_indexing:constraint_classification(data_extraction_regimes, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS USER (SNARE) — Constrained rather than fully trapped: can use privacy tools, limit sharing, or leave platforms. But exit costs are high (social isolation, economic disadvantage, knowledge opacity). Attempts to opt-out are met with dark patterns, friction, and algorithmic suppression of alternatives. Extraction is severe but not maximized because constrained agents retain some agency, albeit at high personal cost.
constraint_indexing:constraint_classification(data_extraction_regimes, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences data extraction as coordination: collecting signals about user behavior enables content recommendation, fraud detection, and service improvement. Benefits massively from the data flow. Has exit options (can diversify, can reduce data collection) and exercises them selectively. Net beneficiary — the constraint subsidizes platform operations. Sees the regime as a problem-solving mechanism, not as extraction.
constraint_indexing:constraint_classification(data_extraction_regimes, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY REGIME (TANGLED ROPE) — Organized state actors (GDPR, FTC, DPA) see the extraction regime and respond with coordination mechanisms (consent requirements, transparency mandates, data subject rights). But enforcement is partial, regulatory arbitrage is rampant, and compliance creates theater rather than actual control. The regime involves genuine coordination (establishing data protection standards) alongside asymmetric extraction (firms still extract far more than consent enables). Mobile exit because regulators can escalate enforcement, ban services, or impose structural remedies.
constraint_indexing:constraint_classification(data_extraction_regimes, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE DATA MARKETPLACE (PITON) — Data brokers and secondary markets once thrived on selling user data (Acxiom, Equifax, data append services). This ecosystem is now largely degraded: GDPR and equivalent regulations have reduced data availability, consent frameworks limit onward sale, and the primary extraction now happens at the source (platforms retain data rather than reselling it). The marketplace persists through inertia and niche use cases but has lost primary function. Theater ratio is high because the secondary market maintains the pretense of 'data trading' while platforms have monopolized the extraction.
constraint_indexing:constraint_classification(data_extraction_regimes, piton,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DATA GOVERNANCE ALTERNATIVE (SCAFFOLD) — Organized coalitions (cooperative data trusts, personal data pod initiatives, open data standards) are building alternative infrastructures where data subjects control their own information flows. Low effective extraction because the coalition has agency and sees a clear sunset: as cooperative data models mature and interoperability standards proliferate, the centralized extraction regime loses its structural necessity. Suppression is declining as alternatives proliferate. Sunset logic applies: the constraint degrades as decentralized architectures mature (estimated 10-20 year horizon).
constraint_indexing:constraint_classification(data_extraction_regimes, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational timescale, some data extraction appears inevitable: platforms must gather signals to improve services, security requires behavioral anomaly detection, and personalization requires understanding user preferences. This perspective risks naturalizing what is actually a contingent design choice: the current extraction regime reflects specific architectural decisions (centralized data storage, opaque algorithms, asymmetric ownership) not immutable requirements. The engine's false summit detector will flag this perspective as a naturalization of institutional choices presented as natural law.
constraint_indexing:constraint_classification(data_extraction_regimes, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_extraction_regimes_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_extraction_regimes, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_extraction_regimes, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_extraction_regimes, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_extraction_regimes, TR),
    TR >= 0.70.

:- end_tests(data_extraction_regimes_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The current regime extracts substantial value from users who receive service access but derive no benefit from their own data monetization. The extraction is not maximized (0.95) because users retain some ability to migrate (slow, costly, but possible) and platforms do invest some portion of extracted value back into service improvements. The rising extractiveness trajectory (0.45 → 0.68 over interval) reflects accumulation: as platforms build larger datasets, behavioral inferences become more accurate and more valuable, enabling higher-margin extraction. Suppression (0.72): High. Structural suppression includes technical opacity (algorithms are proprietary black boxes), informational suppression (terms of service are intentionally incomprehensible), legal suppression (data ownership structures favor platforms), and economic suppression (switching costs are high, network effects lock users in). Internalized suppression is also significant: users have absorbed narratives that 'privacy is dead,' surveillance is inevitable, and data is the price of digital participation. Theater ratio (0.58, rising): Moderate-high. Significant performative activity includes privacy control interfaces (that do not prevent the extraction they appear to control), consent dialogs (that are theatrically required but practically non-negotiable), and transparency disclosures (written in legalese, functionally opaque). However, some genuine coordination theater exists: platforms do use data for service improvement, not purely for monetization. The rising theater trajectory reflects increasing sophistication of consent theater and privacy theater as regulatory pressure increases.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces stark perspectival divergence. Data subjects see an inescapable snare: no viable exit, extraction is non-negotiable, suppression is total (structural and internalized). Platform operators see a rope: the constraint solves the coordination problem of understanding user needs; they derive massive benefit; they experience only the service-improvement function. Regulatory regimes see a tangled rope: they acknowledge genuine coordination (data improves service) while attempting to impose extraction limits (consent requirements, data minimization, user rights), but enforcement is partial. Alternative governance sees a scaffold: the constraint is temporary, emerging architectures (federated identity, user-controlled data stores, cooperative platforms) will eventually replace centralized extraction. The piton perspective reveals degradation: secondary data markets that once thrived (Acxiom, data brokers) are atrophying because primary extraction now happens at platform scale; the secondary market persists through inertia. The analytical observer risks a false summit: naturalizing extraction as inevitable requirement of digital service, when the current regime actually reflects specific architectural and business model choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from each agent's structural position relative to the extraction. Data subjects are victims with trapped exit (no real alternative): d ≈ 0.95, f(d) ≈ 1.42 (maximum experienced extraction). Privacy-conscious users are victims with constrained exit (can leave at high cost): d ≈ 0.75, f(d) ≈ 1.15 (high extraction). Platform operators are beneficiaries with arbitrage exit (can diversify data collection strategies): d ≈ 0.05, f(d) ≈ -0.12 (effective subsidy — constraint runs toward them). Regulatory regimes are mixed — nominally organized monitors but partly captured by platforms they regulate: d ≈ 0.50, f(d) ≈ 0.65 (balanced). Alternative governance coalitions have mobile exit (can build competitive systems): d ≈ 0.45, f(d) ≈ 0.50 (moderate extraction). The platform operator's perspective applies negative f(d) because they are structured beneficiaries — the extraction flows TO them, not FROM them. Their perspective is rope (or coordination) precisely because they experience the constraint as subsidizing their operations.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that snare and rope are both structurally correct for their respective observational contexts. The user (snare) and platform (rope) are measuring the same constraint from positions of opposite structural power. The conflict is not 'which type is true' but 'whose structural position determines the classification.' The snare classification from the user perspective is not false; it accurately describes the user's structural reality. The rope classification from the platform perspective is not false; it accurately describes the platform's structural reality. The mandatrophy dissolves when the presheaf structure is recognized: classification is perspectival, not objective. The regulatory and alternative governance perspectives add important signal: they reveal that the snare-vs-rope gap can be reduced through intervention (regulation, architecture redesign) but not eliminated without fundamentally changing platform business models. The piton perspective indicates which coordination mechanisms (secondary data markets) have already degraded under regulatory pressure. The analytical mountain perspective is false — the current extraction architecture is contingent, not inevitable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_fiction_threshold,
    'At what threshold of incomprehensibility do terms of service constitute non-consensual extraction rather than informed consent?',
    'Readability analysis of privacy policies (Flesch-Kincaid grade level, comprehension testing with representative users); correlation between stated consent and actual reading/understanding behavior',
    'If threshold is low (grade 8): most platform ToS are non-consensual. If threshold is high (grade 16+): platforms can claim validity of consent by compliance. Determines whether extraction is trap (non-consensual) or rope (coordinated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_fiction_threshold, empirical, 'Threshold for distinguishing consensual from non-consensual data extraction via terms of service').

omega_variable(
    alternatives_sufficiency,
    'Do alternative platforms (encrypted messaging, federated social networks, cooperative data models) provide functionally equivalent services that enable exit without material loss?',
    'User adoption rates and retention rates for alternatives; feature parity analysis; network effects measurement (does value of alternative increase with adoption?); cost-benefit analysis for users switching platforms',
    'If alternatives are sufficient: exit option is constrained not trapped, classification downgrades from snare to tangled_rope. If alternatives are insufficient: trapped classification confirmed, extraction severity is high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternatives_sufficiency, empirical, 'Whether functional alternatives exist and enable exit without material loss').

omega_variable(
    data_monetization_necessity,
    'Is the current level and scope of data extraction necessary for platform service delivery and sustainable business models, or does it exceed functional requirements?',
    'Engineering analysis of data minimization: which data attributes are necessary for service function vs. which are collected purely for monetization? Comparison with minimal-data platform designs that deliver equivalent service (e.g., DuckDuckGo vs. Google). Cost allocation: what percentage of platform revenue requires data extraction beyond service delivery?',
    'If extraction exceeds service requirements: confirms snare/extraction classification. If extraction is necessary: reframes as coordination cost, potentially changing perspectives from snare to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_monetization_necessity, empirical, 'Necessity and scope of data extraction relative to service delivery requirements').

omega_variable(
    suppression_internalization_depth,
    'How much of the measured suppression (0.72) is structural (technical barriers, legal frameworks, economic dependency) versus internalized (users believe surveillance is inevitable or deserved)?',
    'Post-exit suppression trajectory: if users maintain secrecy-conscious behaviors after leaving extractive platforms, suppression persists (internalized). If behavior changes radically (oversharing on non-extractive platforms), suppression was primarily structural. Qualitative interviews on perceived inevitability of data extraction.',
    'If internalized: effective suppression is higher than structural measure suggests — users carry the extraction with them. If structural: suppression is purely technical/legal and declines when barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Degree to which suppression is structural vs. internalized in user cognition').

omega_variable(
    regulatory_capture_depth,
    'To what extent has the regulatory regime (perspective 4) been captured by platform operators, weakening its coordination function and leaving asymmetric extraction intact?',
    'Analysis of regulatory outcomes: enforcement patterns (which violations are penalized vs. ignored?), penalty severity (are fines large enough to change behavior?), regulatory timeline (can compliance occur faster than regulatory cycles?). Revolving door analysis (regulator-to-industry movement). Compliance theater measurement (do actual data practices change after regulatory action?).',
    'If capture is high: regulatory perspective is overstating coordination and understating continued extraction. Tangled rope classification becomes closer to snare. If capture is low: regulatory regime is genuine check on extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture within data protection frameworks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_extraction_regimes, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(data_extract_tr_t0, data_extraction_regimes, theater_ratio, 0, 0.35).
narrative_ontology:measurement(data_extract_tr_t5, data_extraction_regimes, theater_ratio, 5, 0.48).
narrative_ontology:measurement(data_extract_tr_t10, data_extraction_regimes, theater_ratio, 10, 0.58).
narrative_ontology:measurement(data_extract_tr_t2, data_extraction_regimes, theater_ratio, 2, 0.4).

% Extraction over time
narrative_ontology:measurement(data_extract_be_t0, data_extraction_regimes, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(data_extract_be_t5, data_extraction_regimes, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(data_extract_be_t10, data_extraction_regimes, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(data_extract_be_t2, data_extraction_regimes, base_extractiveness, 2, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_extraction_regimes, resource_allocation).
narrative_ontology:boltzmann_floor_override(data_extraction_regimes, 0.25).
narrative_ontology:affects_constraint(data_extraction_regimes, algorithmic_recommendation_bias).
narrative_ontology:affects_constraint(data_extraction_regimes, surveillance_capitalism).
narrative_ontology:affects_constraint(data_extraction_regimes, consent_framework_theater).
narrative_ontology:affects_constraint(data_extraction_regimes, personal_data_commodification).

% DUAL FORMULATION NOTE:
% Data extraction regimes decompose into multiple structurally distinct constraints. This story covers the primary extraction mechanism (user data collection and monetization). Downstream constraints include algorithmic bias (how extracted data is used), consent theater (the performative extraction authorization), and commodification (how data is priced and traded). Each has distinct epsilon values reflecting different verification bottlenecks. All are linked via network.affects_constraints to enable contamination propagation analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_extraction_regimes, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
