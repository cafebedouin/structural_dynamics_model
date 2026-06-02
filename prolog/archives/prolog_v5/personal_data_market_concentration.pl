% ============================================================================
% CONSTRAINT STORY: personal_data_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personal_data_market_concentration, []).

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
 *   constraint_id: personal_data_market_concentration
 *   human_readable: Personal Data Market Concentration and Asymmetric Extraction
 *   domain: digital_economy/data_rights/platform_governance
 *
 * SUMMARY:
 *   Personal data market concentration represents a structural constraint
 *   where dominant platforms consolidate control over information flows
 *   necessary for economic and social participation. The constraint exhibits
 *   snare characteristics from the perspective of data subjects (trapped with
 *   no exit options) and competing firms (constrained by asymmetric data
 *   access), while platforms experience it as coordination (network effects
 *   and data utility) and regulators experience it as a mixed
 *   coordination-extraction hybrid (theater-intensive regulation that
 *   validates extraction while performing protection). The extractiveness has
 *   increased over the interval (0.45 to 0.68) as platforms have layered
 *   additional extraction mechanisms (behavioral profiling, attention
 *   commodification, algorithm-mediated discrimination, data-derived
 *   competitive advantages against users' own economic interests) on top of
 *   the initial data collection infrastructure. Theater ratio has similarly
 *   increased (0.35 to 0.58) as regulatory frameworks have grown more complex
 *   and performative: notice-and-consent mechanisms, data portability rights,
 *   and audit requirements create legitimacy theater that masks extraction by
 *   appearing to protect users while making meaningful choice functionally
 *   impossible.
 *
 * KEY AGENTS:
 *   - Individual Data Subjects: Primary victims (powerless/trapped) — structurally dependent on platform participation; bear extraction through behavioral manipulation, discrimination, and attention commodification with no exit capacity
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — extract asymmetric value from data concentration; control access to essential infrastructure; experience constraint as coordination mechanism
 *   - Competing Firms: Secondary victims (moderate/constrained) — data-dependent businesses pay monopoly rates for platform data access or accept competitive disadvantage; can theoretically exit but costs are severe
 *   - Data Protection Regulators & Civil Society: Organized coalition (organized/constrained) — see both coordination function (transparency norms, audit requirements) and extraction (regulatory capture, influence asymmetry); have exit capacity through legislation but face persistent suppression
 *   - Legacy Data Protection Framework: Institutional actor (institutional/constrained) — maintains performative regulation; piton classification reflects degradation of functional protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personal_data_market_concentration, 0.68).
domain_priors:suppression_score(personal_data_market_concentration, 0.72).
domain_priors:theater_ratio(personal_data_market_concentration, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personal_data_market_concentration, extractiveness, 0.68).
narrative_ontology:constraint_metric(personal_data_market_concentration, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(personal_data_market_concentration, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personal_data_market_concentration, snare).
narrative_ontology:human_readable(personal_data_market_concentration, "Personal Data Market Concentration and Asymmetric Extraction").
narrative_ontology:topic_domain(personal_data_market_concentration, "digital_economy/data_rights/platform_governance").

domain_priors:requires_active_enforcement(personal_data_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personal_data_market_concentration, platform_corporations).
narrative_ontology:constraint_victim(personal_data_market_concentration, individual_data_subjects).
narrative_ontology:constraint_victim(personal_data_market_concentration, competing_firms).
narrative_ontology:constraint_victim(personal_data_market_concentration, public_data_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Individual users cannot meaningfully exit the constraint. Participation in digital life (employment, finance, healthcare, social connection) requires data submission to concentrated platforms. Exit costs are prohibitive: loss of economic opportunity, social isolation, reduced access to essential services. No alternative infrastructure exists at comparable scale. The individual bears extraction (behavioral profiling, attention commodification, price discrimination, manipulation) with no compensation or genuine consent mechanism. Maximum experienced extraction — trapped agent with zero degrees of freedom.
constraint_indexing:constraint_classification(personal_data_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPETING FIRM (SNARE) — Data-dependent businesses (retailers, logistics, finance, advertising) are structurally dependent on data from concentrated platforms. Exit is theoretically possible but costs are severe: switching to alternative data sources reduces competitive precision, forgoing platform APIs reduces market reach, building proprietary data infrastructure requires massive capital. The competitive extraction is substantial — dominant platforms charge monopoly rates for data access or use data advantages to compete directly against their clients. Constrained rather than trapped; costs are severe but surmountable for well-capitalized firms.
constraint_indexing:constraint_classification(personal_data_market_concentration, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY COALITION (TANGLED ROPE) — Organized data protection movements (regulators, privacy advocates, civil society) see both coordination and extraction. The constraint coordinates information standards (data transparency requirements, consent forms, audit trails) that benefit platforms by creating legal safe harbors while simultaneously creating theater that masks extraction. The coalition faces suppression through regulatory capture, industry lobbying, and the technical complexity advantage of platforms. But the coalition has exit capacity through legislative pressure, international coordination (GDPR, CCPA), and norm-setting — a mixed experience of both coordination benefit (legitimacy framework) and extraction cost (influence asymmetry).
constraint_indexing:constraint_classification(personal_data_market_concentration, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM CORPORATION (ROPE) — For dominant platforms, the constraint functions as pure coordination. The concentration of data enables network effects, personalization, and targeted service delivery — genuine coordination benefits. The extraction (from data subjects) is experienced by the platform as rightful value capture for providing communication infrastructure. The platform sees the market concentration as solving the coordination problem: fragmented data services would reduce network value. The platform experiences low suppression because it has maximum exit capacity: it can exit regulatory jurisdictions, adjust business models, invest in new data collection. The constraint appears as coordinating the digital commons, not extracting from it.
constraint_indexing:constraint_classification(personal_data_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY DATA PROTECTION FRAMEWORK (PITON) — Early data protection regulations (fair information practices, notice-and-consent, data portability rights) are substantially theatrical and degraded. Theater_ratio is high because consent mechanisms are performative (users cannot meaningfully process or evaluate data terms), data portability is technically complex to the point of uselessness, and notice-and-choice architecture presupposes rational actors with time to evaluate terms. The framework persists through institutional inertia and industry preference for the appearance of regulation over functional constraint. It coordinates neither effectively nor extracts maximally — it validates extraction while performing regulation. Maintained because full transparency/portability would trigger political crisis, not because it functions.
constraint_indexing:constraint_classification(personal_data_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some data concentration may appear immutable: network effects mathematically favor consolidation, data valuation requires scale, algorithmic optimization benefits from large datasets. This perspective risks naturalizing the concentration as inherent to digital technology. However, the structural data contradicts the mountain classification. The concentration is contingent on specific regulatory choices (intellectual property law, merger approval, platform liability exemptions), business model choices (advertising-dependent rather than subscription), and network effects that are socially constructed rather than physically immutable. The engine's false summit detector will identify this as naturalization of contingent institutional arrangements.
constraint_indexing:constraint_classification(personal_data_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personal_data_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(personal_data_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(personal_data_market_concentration, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(personal_data_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(personal_data_market_concentration, TR),
    TR >= 0.70.

:- end_tests(personal_data_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint extracts through multiple mechanisms: (1) behavioral profiling and surveillance creating informational asymmetry; (2) attention commodification converting user engagement into advertiser revenue with no user compensation; (3) algorithmic discrimination in pricing, credit, employment, and service access; (4) competitive data advantages where platforms use user data to compete against their own business partners; (5) lock-in effects where individual users cannot exit without losing economic opportunity. The trajectory from 0.45 to 0.68 reflects layering of extraction mechanisms and the maturation of data monetization strategies. Suppression (0.72): High. Barriers to exit include: genuine technical necessity of platform participation for digital participation in employment, finance, healthcare, social connection; absence of practical alternatives at comparable scale; hidden data flows that users cannot monitor or control; terms-of-service complexity that obscures extraction mechanisms; regulatory capture that prevents functional constraints on platforms. Theater ratio (0.58): Moderate-high and increasing. Regulatory theater is substantial: consent mechanisms (notice-and-choice) are performative because users cannot meaningfully evaluate or act on disclosure; data portability rights are technically complex to uselessness; audit and transparency requirements create legitimacy appearance while platforms retain control over data use. Theater has increased as regulatory complexity has grown — more rules create more appearance of control without increasing actual constraint on extraction.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies in how each agent experiences the same structural constraint. Platforms genuinely experience network effects and data utility — the coordination functions are real and valuable. Users experience asymmetric extraction with no coordination benefit from their perspective — the services they receive do not compensate for behavioral manipulation and algorithmic discrimination. Regulators experience a hybrid: they are building coordination frameworks (transparency, audit, rights) that create legitimacy but are embedded in theater that validates rather than constrains extraction. The analytical observer risks the false summit: concluding that concentration is immutable because network effects mathematically favor consolidation, when the structural data reveals the consolidation as contingent on specific regulatory choices (intellectual property law, merger policy, platform liability exemptions) and business model choices (advertising-dependent rather than subscription or public utility models).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from beneficiary/victim declarations and exit options. Platform corporations as beneficiaries with arbitrage exit options (can move to new markets, adjust business models, invest in new data collection) derive d ≈ 0.05, producing low/negative f(d) — they experience the constraint as enabling, not extractive. Individual data subjects as victims with trapped exit options (no practical alternatives for digital participation) derive d ≈ 0.95, producing high f(d) ≈ 1.42 — they experience maximum extraction. Competing firms as secondary victims with constrained exit options derive d ≈ 0.75, producing f(d) ≈ 1.15. Regulators as organized agents with constrained but real exit capacity (legislative and norm-setting leverage) derive d ≈ 0.55, producing f(d) ≈ 0.75. The scope modifier σ(S) applies at global scope (σ = 1.2), amplifying the effective extraction chi for all perspectives because the concentration operates across all major digital ecosystems globally.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The classification as snare is robust to perspectival variation. From the data subject perspective (powerless/trapped), the constraint is unambiguously snare: high extraction (χ ≈ 0.96), minimal coordination benefit, maximum suppression. From the competing firm perspective (moderate/constrained), it also classifies as snare: extraction is high though somewhat lower (χ ≈ 0.80), minimal coordination benefit from their structural position, significant suppression. From the platform perspective (institutional/arbitrage), the classification would be rope by the metrics alone (the beneficiary experiences coordination), but this reveals a perspectival asymmetry: the same structural constraint is coordination for extractors and snare for victims. The mandatrophy is resolved by recognizing that snare is the engine's canonical classification because the constraint's existence depends on suppressing victim exit options. If victims could freely exit without cost, the extraction would immediately collapse — this is the snare definition. The coordination experienced by platforms would vanish if they could not extract from captive users. The constraint's persistence depends on suppression of victim choice, making it fundamentally snare rather than rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_inevitability,
    'Are data network effects mathematically inevitable or contingent on specific platform architecture choices?',
    'Comparative analysis of federated vs centralized architectures; measurement of network value retention under interoperability; historical analysis of alternative data market structures (credit bureaus, telecom data sharing, health information exchanges)',
    'If inevitable: market concentration is a natural law, snare classification is contingent on business models. If contingent: concentration is a choice, snare classification stands regardless of architecture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_inevitability, empirical, 'Whether network effects driving data concentration are inevitable or contingent').

omega_variable(
    consent_mechanism_functionality,
    'Can notice-and-choice data consent be made functionally meaningful, or is the mechanism fundamentally theater?',
    'Experimental evaluation of consent comprehension under different disclosure formats; measurement of actual decision-making behavior when meaningful alternatives are presented; longitudinal tracking of users with genuine choice capacity (sophisticated investors, technologists) vs general population',
    'If achievable: legacy framework piton classification is premature, framework could evolve toward rope. If theater: framework classification confirmed, suppression of true choice mechanisms is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_functionality, empirical, 'Whether meaningful data consent mechanisms are functionally achievable').

omega_variable(
    alternative_infrastructure_viability,
    'Are decentralized, federated, or public-option data architectures technically and economically viable at scale?',
    'Technology readiness assessment of decentralized identity, federated learning, data trusts, public data infrastructure; cost-benefit analysis vs centralized platforms; pilot program outcomes (EU data spaces, city-level data commons)',
    'If viable: trapped classification becomes constrained, exit options materialize, snare perspective weakens. If unviable: trap remains absolute, suppression measure (0.72) is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_infrastructure_viability, empirical, 'Whether alternative data infrastructure architectures are viable at scale').

omega_variable(
    data_subject_collective_action,
    'Can individual data subjects achieve collective exit capacity through data unions, cooperatives, or coordinated withholding?',
    'Measurement of union/cooperative formation rates and member retention; analysis of data-withholding campaigns and platform response; comparison of collective bargaining outcomes vs individual consent',
    'If achievable: powerless classification could be upgraded to organized, snare becomes tangled rope. If blocked: suppression of coalition formation is part of extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(data_subject_collective_action, empirical, 'Whether data subjects can achieve collective exit capacity through organized action').

omega_variable(
    regulatory_capture_persistence,
    'Is the regulatory coalition''s constrained exit capacity durable, or does concentrated platform power prevent effective data protection regulation?',
    'Longitudinal analysis of regulatory outcomes vs industry lobbying expenditure; measurement of regulatory authority independence from platform influence; comparison of enforcement outcomes across jurisdictions with varying institutional autonomy',
    'If durable: coalition''s tangled rope classification holds, coalition has genuine leverage. If blocked: coalition is degraded piton, regulation persists as theater masking permanent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_persistence, empirical, 'Whether regulatory authority can sustain independence from platform influence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personal_data_market_concentration, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdmc_tr_t0, personal_data_market_concentration, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pdmc_tr_t7, personal_data_market_concentration, theater_ratio, 7, 0.48).
narrative_ontology:measurement(pdmc_tr_t15, personal_data_market_concentration, theater_ratio, 15, 0.58).

% Extraction over time
narrative_ontology:measurement(pdmc_be_t0, personal_data_market_concentration, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(pdmc_be_t7, personal_data_market_concentration, base_extractiveness, 7, 0.58).
narrative_ontology:measurement(pdmc_be_t15, personal_data_market_concentration, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personal_data_market_concentration, resource_allocation).
narrative_ontology:affects_constraint(personal_data_market_concentration, algorithmic_discrimination).
narrative_ontology:affects_constraint(personal_data_market_concentration, attention_economy_lock_in).
narrative_ontology:affects_constraint(personal_data_market_concentration, regulatory_capture_data_governance).
narrative_ontology:affects_constraint(personal_data_market_concentration, digital_identity_centralization).

% DUAL FORMULATION NOTE:
% Personal data market concentration operates as a constraint family. Decomposed into: (1) structural data concentration (this story) with ε=0.68 (snare); (2) algorithmic discrimination using concentrated data (ε=0.62, snare); (3) attention commodification mechanisms (ε=0.55, tangled rope with coordination function); (4) regulatory capture preventing data protection (ε=0.72, snare). Each story has distinct extraction mechanisms, though all depend on the underlying data concentration. Upstream: digital_identity_centralization (ε=0.45, tangled rope) — the initial consolidation of identity infrastructure that enabled data concentration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(personal_data_market_concentration, institutional, 0.05).
constraint_indexing:directionality_override(personal_data_market_concentration, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
