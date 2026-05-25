% ============================================================================
% CONSTRAINT STORY: cross_border_data_flows
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_cross_border_data_flows, []).

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
 *   constraint_id: cross_border_data_flows
 *   human_readable: Cross-Border Data Flow Constraints in Global Digital Infrastructure
 *   domain: digital_governance/international_regulation
 *
 * SUMMARY:
 *   Cross-border data flows represent a core tension in global digital
 *   governance between the technical and economic imperative for
 *   interconnected systems and the regulatory demand for data protection,
 *   sovereignty, and localization. This constraint exhibits multiple
 *   classification types from different structural positions: individual data
 *   subjects experience pure extraction (snare) with no exit; regulating
 *   jurisdictions face mixed coordination and extraction (tangled_rope) as
 *   they attempt to enforce protection while benefiting from digital
 *   services; platform operators see coordination (rope) enabling their
 *   business model; data-exporting nations face dependency dynamics
 *   (tangled_rope); the data protection regulatory regime is substantially
 *   theatrical (piton); and from a civilizational view, some analysts
 *   naturalize the constraint as inherent to networked systems (mountain).
 *   The rising extractiveness trajectory (0.35→0.58 over the interval)
 *   reflects accumulating platform concentration and regulatory arbitrage.
 *   The theater ratio increase (0.45→0.62) indicates that nominal data
 *   protection mechanisms (consent, privacy policies, data minimization
 *   rules) are increasingly performative relative to actual data flows.
 *
 * KEY AGENTS:
 *   - Individual Data Subjects: Primary victims (powerless/trapped) — personal data flows globally without meaningful consent or exit mechanisms
 *   - Regulating Jurisdictions: Secondary victims (moderate/constrained) — attempt to enforce protection standards but face platform leverage and jurisdictional limits
 *   - Tech Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture value from frictionless global data flows; have regulatory workarounds
 *   - Data-Exporting Nations: Secondary beneficiaries/victims (organized/constrained) — benefit from global platform access but face data sovereignty erosion and dependency
 *   - Privacy Advocates & DPAs: Organized challengers (organized/constrained) — attempt to enforce regulations but constrained by platform dominance and international coordination failures
 *   - Competing Data Markets: Implicit victims — alternative data ecosystems (European cloud, sovereign cloud, decentralized systems) cannot compete against incumbent global platforms due to network effects
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(cross_border_data_flows, 0.58).
domain_priors:suppression_score(cross_border_data_flows, 0.65).
domain_priors:theater_ratio(cross_border_data_flows, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cross_border_data_flows, extractiveness, 0.58).
narrative_ontology:constraint_metric(cross_border_data_flows, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(cross_border_data_flows, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(cross_border_data_flows, tangled_rope).
narrative_ontology:human_readable(cross_border_data_flows, "Cross-Border Data Flow Constraints in Global Digital Infrastructure").
narrative_ontology:topic_domain(cross_border_data_flows, "digital_governance/international_regulation").

domain_priors:requires_active_enforcement(cross_border_data_flows).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(cross_border_data_flows, tech_platform_operators).
narrative_ontology:constraint_beneficiary(cross_border_data_flows, data_exporting_nations).
narrative_ontology:constraint_victim(cross_border_data_flows, data_subjects).
narrative_ontology:constraint_victim(cross_border_data_flows, regulating_jurisdictions).
narrative_ontology:constraint_victim(cross_border_data_flows, competing_data_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Trapped with no exit. Personal data flows across borders independent of individual consent mechanisms; jurisdiction of origin cannot enforce protection once data leaves. Individual subject cannot opt out of global data infrastructure without withdrawing from digital life entirely. Maximum suppression — no technical or legal mechanism provides exit below catastrophic cost.
constraint_indexing:constraint_classification(cross_border_data_flows, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGULATING JURISDICTION (TANGLED ROPE) — Constrained by platform dominance and data exporter leverage, but also benefits from data-driven services and network effects. Enforcement is costly (GDPR compliance burdens, DPA resources) and effectiveness is limited by jurisdictional boundaries. Both coordination function (standard-setting, cross-border agreements) and extraction (platform compliance arbitrage, data sovereignty erosion) present.
constraint_indexing:constraint_classification(cross_border_data_flows, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECH PLATFORM OPERATOR (ROPE) — Benefits from frictionless data flows. Experiences the constraint as pure coordination problem: communicating data across borders enables service delivery. Has arbitrage options (data localization workarounds, regulatory arbitrage, technical circumvention). Net beneficiary experiencing low extraction.
constraint_indexing:constraint_classification(cross_border_data_flows, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DATA-EXPORTING NATION (TANGLED ROPE) — Organized state actor constrained by platform dependency and digital supply chain integration. Benefits from access to global talent pools, research data, and service platforms. Also bears costs through sovereign data leverage erosion and brain drain. Coordination function (interoperability agreements) alongside extraction (dependency lock-in).
constraint_indexing:constraint_classification(cross_border_data_flows, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: DATA PROTECTION THEATER (PITON) — Privacy regulation (GDPR, CCPA, consent banners) is largely performative. The infrastructure mechanisms (encryption, anonymization, data minimization) exist but enforcement is theatrical — compliance is often cosmetic, consent is illusory (dark patterns), and data continues flowing regardless of nominal restrictions. Theater ratio high because regulatory performance persists despite low functional protection.
constraint_indexing:constraint_classification(cross_border_data_flows, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some data flow is inherent to global digital infrastructure: network effects demand interoperability, and data portability is structurally entangled with service delivery. This perspective risks treating contingent institutional arrangements (platform dominance, regulatory fragmentation, asymmetric data extraction) as immutable laws of networked systems. Engine false summit detection applies.
constraint_indexing:constraint_classification(cross_border_data_flows, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_border_data_flows_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(cross_border_data_flows, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_border_data_flows, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(cross_border_data_flows, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(cross_border_data_flows, TR),
    TR >= 0.70.

:- end_tests(cross_border_data_flows_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platform operators extract significant value through data monetization, behavioral targeting, and service lock-in. The extraction is not maximal (0.58 vs. 0.72+) because genuine coordination functions exist (data portability enabling service switching, interoperability standards creating competition) and because regulatory pressure does create some friction. The trajectory from 0.35 to 0.58 reflects increasing concentration of data in fewer platforms. Suppression (0.65): High. Multiple barriers prevent data subjects from exiting: technical lock-in (social graph, network effects), economic dependency (free services replacing paid alternatives), regulatory barriers (GDPR creates compliance cost but not protection), and identity factors (discussed in omegas). Importantly, suppression is not evenly distributed — wealthy individuals and privacy-conscious professionals can implement technical privacy measures; powerless individuals face structural suppression. Theater ratio (0.62): Moderate-high. GDPR, CCPA, and privacy regulations create substantial compliance bureaucracy (privacy officers, consent management platforms, data impact assessments) but do not prevent data flows. The theater consists of: (1) consent mechanisms that are illusory (dark patterns, pre-ticked consent, impossible opt-out), (2) data minimization rules that are routinely circumvented through technical means (fingerprinting, inferred data), (3) deletion rights that are nominal (data is 'anonymized' but often re-identifiable). This is not to say regulations are useless — they do create enforcement points and economic cost for non-compliance. But the primary function has shifted from preventing extraction to creating compliance theater.
 *
 * PERSPECTIVAL GAP:
 *   The largest gap is between platforms (rope: low extraction experienced) and data subjects (snare: maximum extraction experienced). Both are experiencing the same structural extractiveness (ε=0.58), but platforms' low d value produces near-zero chi while subjects' high d value produces chi > 0.8. This gap is the diagnostic signature of asymmetric extraction — the constraint serves one agent while harming another, not from disagreement about classification but from genuine structural asymmetry in who bears the costs. Secondary gaps exist between regulating jurisdictions (tangled_rope: mixed costs/benefits) and both poles of the primary gap. The piton classification (data protection theater) and mountain classification (natural law view) represent meta-level gaps where the agent is no longer operating within the constraint but observing it — piton sees the constraint as degraded, mountain sees it as inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint is determined by structural position in the data extraction pipeline. Data subjects with trapped exit options have d ≈ 0.95 (maximum targets). Regulating jurisdictions with constrained exit (dependent on platforms for economic growth, innovation) have d ≈ 0.60 (moderate targets). Tech platforms with arbitrage options (regulatory arbitrage, data localization workarounds, jurisdictional shopping) have d ≈ 0.10 (beneficiaries). Data-exporting nations with constrained but not trapped exit (can theoretically mandate data localization but face brain drain and service degradation) have d ≈ 0.50 (ambiguous). The sigmoid f(d) applies these d values to base extractiveness, producing effective extraction chi that varies by perspective. Data subjects experience the highest chi because f(d) amplifies their d ≈ 0.95. Platforms experience the lowest chi because their d ≈ 0.10 produces negative or near-zero f(d). Scope modifier σ(S) scales chi by scope: cross-border flows are global (σ=1.2), amplifying effective extraction. This explains why the same extractiveness value (0.58) produces different experienced extraction across perspectives — the mathematical machinery (chi = ε × f(d) × σ(S)) distributes the extraction unevenly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the mandatrophy at moderate intensity. The analytical observer risks false naturalization (mountain) by treating platform dominance and regulatory fragmentation as inherent to networked systems. The structural data contradicts this: data localization is technically feasible (Russia, China, EU operate local infrastructure); network effects create strong coordination benefits but are not logically necessary (decentralized alternatives exist, though they have higher friction); regulatory fragmentation is contingent (GDPR, CCPA, China's cybersecurity law represent policy choices, not natural laws). The mandatrophy resolution involves: (1) recognizing that 'inherent to digital infrastructure' naturalizes institutional choices (platform concentration, regulatory arbitrage, consent theater), (2) decomposing the constraint into its coordination function (genuine interoperability, service portability) and its extraction mechanism (data monopoly, behavioral targeting, lock-in), (3) identifying whether alternatives exist and at what cost (local-first architecture: technically feasible, economically costly; decentralized systems: viable for some use cases, not others; regulatory harmonization: possible but not imminent). The tangled_rope classification acknowledges both functions while resisting the naturalization. The theater_ratio increase reveals that regulation is becoming more performative over time, not more effective — this is a leading indicator of constraint degradation into piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_extraction_vs_coordination,
    'Is the measured extractiveness primarily driven by platform monopoly extraction or by coordination costs inherent to cross-border interoperability?',
    'Decomposition of extractiveness into platform-specific costs vs. general infrastructure costs; comparison of extractiveness under competitive vs. monopolistic platform structures',
    'If monopoly-driven: platform power (d ≈ 0.9) makes extraction the primary mechanism, snare classification dominates. If coordination-driven: network effects justify higher baseline extraction as legitimate infrastructure cost, tangled_rope becomes the primary type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_extraction_vs_coordination, empirical, 'Platform monopoly extraction vs. genuine coordination cost').

omega_variable(
    consent_mechanism_validity,
    'Does current consent-based data protection (GDPR consent, privacy policies) provide genuine exit options or is it purely theatrical?',
    'Analysis of consent withdrawal rates; tracking of actual data deletion post-withdrawal; comparison of stated vs. actual data retention practices; dark pattern audit methodology',
    'If valid: suppression ≤ 0.50, exit options include ''constrained'' rather than ''trapped''. If theatrical: suppression ≥ 0.70, trapped exit dominates, snare classification strengthens across all victim perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_validity, empirical, 'Whether consent mechanisms provide genuine data protection or are performative').

omega_variable(
    regulatory_balkanization_inevitability,
    'Is fragmented data regulation (GDPR vs. China''s data sovereignty vs. US permissiveness) an unavoidable outcome of national sovereignty or a contingent institutional arrangement that could be globally harmonized?',
    'Historical analysis of other international standards harmonization (financial, telecommunications, environmental); comparison of regulatory convergence pressure vs. divergence incentives; game-theoretic analysis of incentive structures for harmonization',
    'If inevitable: regulatory arbitrage is a structural feature of global data flows, extraction is harder to reduce. If contingent: harmonization could lower suppression and extractiveness by removing regulatory fragmentation as a differentiator.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_balkanization_inevitability, conceptual, 'Whether data regulatory fragmentation is inevitable or contingently chosen').

omega_variable(
    data_sovereignty_alternative_feasibility,
    'Can data localization and national data sovereignty approaches provide genuine alternative infrastructure with acceptable coordination costs, or do network effects make global flows structurally necessary?',
    'Technical feasibility analysis of local-first architecture; cost modeling of redundant infrastructure; empirical study of countries with data localization mandates (Russia, China, India) and service quality/cost metrics',
    'If feasible alternatives exist: scaffold perspective is valid, sunset clause is real (data localization could replace extraction over time). If network effects enforce global flows: mountain or snare perspective dominates, alternatives are aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_sovereignty_alternative_feasibility, empirical, 'Feasibility of data localization as alternative to global flows').

omega_variable(
    identity_locked_data_subject,
    'Are data subjects trapped by material barriers (technical inability to communicate without platforms) or by identity fusion (self-concept constituted through digital participation)?',
    'Ethnographic study of digital exit attempts; analysis of communities with low digital engagement; comparison of material barriers vs. psychological/social barriers to exit',
    'If material barriers dominant: trapped exit classification and snare remain appropriate. If identity fusion dominant: reclassify as identity_locked; this reveals that the suppression mechanism is internalized, persisting even when material barriers are removed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_data_subject, empirical, 'Whether data subject entrapment is structural or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(cross_border_data_flows, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cbdf_tr_t0, cross_border_data_flows, theater_ratio, 0, 0.45).
narrative_ontology:measurement(cbdf_tr_t10, cross_border_data_flows, theater_ratio, 10, 0.55).
narrative_ontology:measurement(cbdf_tr_t20, cross_border_data_flows, theater_ratio, 20, 0.62).
narrative_ontology:measurement(cbdf_tr_t5, cross_border_data_flows, theater_ratio, 5, 0.5).

% Extraction over time
narrative_ontology:measurement(cbdf_be_t0, cross_border_data_flows, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cbdf_be_t10, cross_border_data_flows, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cbdf_be_t20, cross_border_data_flows, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cbdf_be_t5, cross_border_data_flows, base_extractiveness, 5, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(cross_border_data_flows, global_infrastructure).
narrative_ontology:boltzmann_floor_override(cross_border_data_flows, 0.22).
narrative_ontology:affects_constraint(cross_border_data_flows, data_sovereignty_mandate).
narrative_ontology:affects_constraint(cross_border_data_flows, platform_dominance_extraction).
narrative_ontology:affects_constraint(cross_border_data_flows, regulatory_arbitrage_mechanism).

% DUAL FORMULATION NOTE:
% Cross-border data flows decompose into three related but structurally distinct constraints: (1) data_sovereignty_mandate (ε≈0.45) focusing on state-level regulatory enforcement and jurisdictional fragmentation; (2) platform_dominance_extraction (ε≈0.72) focusing on bilateral extraction between platforms and users; (3) regulatory_arbitrage_mechanism (ε≈0.55) focusing on how platforms exploit differences between regulatory regimes. This story captures the integrated system view; see separate stories for domain-specific analyses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(cross_border_data_flows, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
