% ============================================================================
% CONSTRAINT STORY: data_monetization_asymmetry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_data_monetization_asymmetry, []).

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
 *   constraint_id: data_monetization_asymmetry
 *   human_readable: Data Monetization Asymmetry in Platform Ecosystems
 *   domain: digital_economy/information_asymmetry
 *
 * SUMMARY:
 *   Data monetization asymmetry is a structural constraint that emerges from
 *   the economic fundamentals of digital platforms: user attention and
 *   behavioral data are valuable; users generate data as a byproduct of
 *   participation; platforms capture and monetize this data without
 *   compensating users; regulatory and competitive barriers prevent users
 *   from capturing the value they generate. The constraint exhibits genuine
 *   coordination function (platforms do solve discovery, matching, and
 *   efficiency problems that would be difficult to solve otherwise) alongside
 *   genuine extraction (data subjects receive no compensation and face high
 *   barriers to exit). This dual structure — coordination + asymmetric
 *   extraction — defines the tangled rope classification. The constraint's
 *   theater ratio (0.55) reflects the performative character of privacy
 *   governance: privacy policies, consent mechanisms, and data protection
 *   disclosures satisfy legal requirements while minimizing actual
 *   constraints on data monetization. The extractiveness trajectory (0.35 →
 *   0.58 over 10 years) reflects the accumulating asymmetry as platforms
 *   consolidate market power, expand data collection, and refine monetization
 *   techniques. The suppression (0.68) reflects both structural barriers
 *   (network effects, switching costs, absence of functional alternatives)
 *   and institutional barriers (dark patterns, obscured opt-out paths,
 *   contractual lock-in).
 *
 * KEY AGENTS:
 *   - Data Subject: Primary victim (powerless/trapped) — generates data through participation; receives no compensation; trapped by network effects and service dependency
 *   - Platform Operator: Primary beneficiary (institutional/arbitrage) — owns data collection infrastructure; monetizes data through targeted advertising, algorithmic leverage, and data sales; can shift monetization strategies and operate across jurisdictions
 *   - Data-Generating Worker: Secondary victim (moderate/constrained) — benefits from platform access and reach while bearing extraction through profile monetization and algorithmic suppression
 *   - Competing Platform Operator: Secondary victim (institutional/constrained) — faces crushing disadvantage due to data lock-in despite benefiting from the data monetization revenue model
 *   - Regulatory Coalition: Organized agent (organized/constrained) — governments, data protection authorities, consumer advocates building alternative governance pathways (data portability, algorithmic transparency, consent rights)
 *   - Privacy Theatre Regime: Institutional theater (institutional/arbitrage) — privacy policies, consent mechanisms, and compliance disclosures maintain legitimacy while minimizing functional constraints on monetization
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks misclassifying the constraint as either pure coordination (rope) or pure extraction (snare) without recognizing the tangled hybrid
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(data_monetization_asymmetry, 0.58).
domain_priors:suppression_score(data_monetization_asymmetry, 0.68).
domain_priors:theater_ratio(data_monetization_asymmetry, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(data_monetization_asymmetry, extractiveness, 0.58).
narrative_ontology:constraint_metric(data_monetization_asymmetry, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(data_monetization_asymmetry, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(data_monetization_asymmetry, tangled_rope).
narrative_ontology:human_readable(data_monetization_asymmetry, "Data Monetization Asymmetry in Platform Ecosystems").
narrative_ontology:topic_domain(data_monetization_asymmetry, "digital_economy/information_asymmetry").

domain_priors:requires_active_enforcement(data_monetization_asymmetry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(data_monetization_asymmetry, platform_operators).
narrative_ontology:constraint_beneficiary(data_monetization_asymmetry, data_aggregators).
narrative_ontology:constraint_beneficiary(data_monetization_asymmetry, algorithmic_decision_makers).
narrative_ontology:constraint_victim(data_monetization_asymmetry, data_subjects).
narrative_ontology:constraint_victim(data_monetization_asymmetry, information_commons).
narrative_ontology:constraint_victim(data_monetization_asymmetry, competitive_newcomers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Cannot exit participation in platform ecosystems without foregoing essential services (communications, commerce, employment search, financial inclusion). Data extraction flows relentlessly; suppression is structural (no practical alternative platforms with equivalent network effects). Maximum experienced extraction. The subject is trapped by network effects and service dependency, not by explicit barriers but by absence of functional alternatives.
constraint_indexing:constraint_classification(data_monetization_asymmetry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DATA-GENERATING WORKER (TANGLED ROPE) — Platform gig workers, content creators, and service providers generate data while accessing the coordination platform. They benefit from reach and audience access (genuine coordination function) while bearing asymmetric extraction through profile monetization, algorithmic suppression of earnings, and inability to port reputation signals. Constrained exit due to platform-specific reputation lock and income dependency. Mixed coordination and extraction.
constraint_indexing:constraint_classification(data_monetization_asymmetry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the constraint as a coordination mechanism: aggregating, structuring, and monetizing data enables matching and efficiency gains. Network effects create value that couldn't exist without centralized data collection and analysis. The operator has arbitrage options (can shift monetization strategies, sell to competitors, license data selectively). Data collection is framed as essential coordination infrastructure rather than extraction.
constraint_indexing:constraint_classification(data_monetization_asymmetry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Data protection regulations (GDPR, CCPA, emerging AI governance) establish sunset mechanisms: data portability rights, algorithmic transparency requirements, consent revocation, and data minimization principles are building alternative governance pathways. The coalition has agency (legislative power) but faces resistance from entrenched platform interests. Effective extraction is moderate because the coalition perceives and is building a transition mechanism, even if implementation lags far behind the ideal.
constraint_indexing:constraint_classification(data_monetization_asymmetry, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: COMPETING PLATFORM OPERATOR (TANGLED ROPE) — New entrants benefit from data monetization norms (expect to extract value from their users) but face crushing disadvantage: existing platforms have accumulated vast datasets and can cross-subsidize services with existing revenue streams. The constraint both enables and constrains: data monetization creates a revenue model for the newcomer (coordination benefit) while simultaneously trapping them in an extraction race they cannot win (asymmetric extraction). High suppression due to data lock-in.
constraint_indexing:constraint_classification(data_monetization_asymmetry, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: PRIVACY THEATRE REGIME (PITON) — Privacy policies, data use disclosures, and opt-out mechanisms are substantially performative: notices are written to satisfy legal requirement rather than to communicate meaningfully; opt-out paths are deliberately obscured; consent is often pre-checked or bundled with service terms. The theatrical compliance persists through institutional inertia — platforms maintain privacy theater because abandoning it entirely would trigger regulatory intervention, but the functional value of the theater in protecting data subjects is minimal.
constraint_indexing:constraint_classification(data_monetization_asymmetry, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational standpoint, data monetization simultaneously solves a coordination problem (how to fund global digital infrastructure) and creates an extraction mechanism (asymmetric capture of value generated by data subjects). The constraint exhibits genuine coordination function (platforms do solve matching, discovery, and efficiency problems) alongside genuine extraction (data subjects receive no compensation proportional to their data's economic value, and barriers to exit are structural). Classification as tangled rope reflects this dual structure — neither pure coordination nor pure extraction, but hybrid.
constraint_indexing:constraint_classification(data_monetization_asymmetry, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(data_monetization_asymmetry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(data_monetization_asymmetry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(data_monetization_asymmetry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(data_monetization_asymmetry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(data_monetization_asymmetry, TR),
    TR >= 0.70.

:- end_tests(data_monetization_asymmetry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over time. Platform data monetization is not total extraction (users do receive valuable services — coordination benefit exists) but the value capture asymmetry is severe. Users generate data worth billions in advertising and trading value while receiving services valued at approximately zero in nominal terms (though perhaps higher in subjective utility). The trajectory from 0.35 to 0.58 reflects platform consolidation and refinement of monetization techniques (algorithmic targeting, predictive analytics, federated data sales) that increase extraction relative to service value. Suppression (0.68): High, reflecting both structural and institutional barriers. Structural: network effects mean alternatives cannot achieve functional equivalence without massive user adoption; switching costs are real (reputation, social graphs, habit). Institutional: privacy dark patterns, deliberately obscured data export tools, and contractual language designed to maximize lock-in add institutional suppression on top of structural barriers. Theater ratio (0.55): Moderate, reflecting that privacy governance is partly theatrical but not entirely. Privacy impact assessments, data minimization principles, and some genuine consent mechanisms exist but are offset by performative compliance, pre-checked consent, and bundled terms. The trajectory from 0.30 to 0.55 reflects that platforms have become more sophisticated in theatrical compliance as regulatory scrutiny has increased.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural mechanism appears as coordination (rope) to beneficiaries and as extraction (snare) to victims. The platform operator truthfully observes that data collection enables valuable services — matching, discovery, personalization, efficiency. The data subject truthfully observes that they are extracting value without compensation and have no alternative. Both observations are correct; the classification depends on the observer's structural position. The scaffold perspective reveals that regulatory coalitions perceive a sunset mechanism (data portability, algorithmic transparency, federated data models) that could restructure the constraint. The piton perspective reveals that privacy theatre maintains the constraint's legitimacy without protecting data subjects functionally — the regulatory ritual is degraded but persists through institutional inertia. The analytical observer must recognize that the constraint is genuinely hybrid (coordination + extraction) rather than choosing between rope and snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective is derived from structural position within the constraint. Platform operators are beneficiaries with arbitrage options (exit to alternative business models, jurisdictions, or acquisition is possible) — low d, low experienced extraction. Data subjects are victims with trapped exit options (no functional alternatives due to network effects and service dependency) — high d, high experienced extraction. Data-generating workers are mixed: they benefit from platform access (partial beneficiary) but face extraction through asymmetric value capture (partial victim); constrained exit options (can exit but face significant costs) produce moderate d and moderate experienced extraction. Competing platform operators are victims (face disadvantage relative to incumbents) with constrained exit (cannot build competing platforms without accepting data disadvantage) — moderate-high d, moderate-high experienced extraction. Regulatory coalition is organized (has legislative power) with constrained exit (cannot abandon jurisdiction but can shift enforcement priorities) — moderate d, moderate experienced extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that tangled rope is the accurate classification: the constraint genuinely coordinates (platforms solve real matching and efficiency problems) AND genuinely extracts (data subjects receive no compensation for their data and face high barriers to exit). Neither rope nor snare alone captures the structure. The mandatrophy would occur if we tried to classify this as pure rope (ignoring asymmetric extraction) or pure snare (ignoring genuine coordination function). The beneficiary/victim declarations make the hybrid structure explicit: beneficiaries include platform operators and data aggregators (they capture monetization value); victims include data subjects and information commons (they bear the extraction cost while contributing the data generating the value). The enforcement requirement (`requires_active_enforcement: true`) reflects that the constraint's asymmetry would not persist without active mechanisms (contractual lock-in, dark patterns, regulatory arbitrage) that maintain beneficiary advantage. The scaffold perspective provides the sunset mechanism: data governance reforms, data portability standards, and algorithmic transparency requirements are structurally modifying the constraint, though implementation lags significantly behind the regulatory intent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_value_decomposition,
    'What fraction of platform value derives from coordination function (matching, efficiency, network effects) versus from data monetization (behavioral targeting, algorithmic leverage, information asymmetry)?',
    'Counterfactual analysis: construct platform services with minimal data monetization (data minimization, no algorithmic targeting, no third-party data sales) and measure user willingness-to-pay or engagement relative to baseline platforms. A/B testing on privacy settings and transparency.',
    'If coordination > 60%: snare classification weakens — genuine value creation justifies some extraction. If monetization > 60%: snare dominates — the coordination frame is primarily cover story. Affects beneficiary/victim assignment at institutional level.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_value_decomposition, empirical, 'Decomposition of platform value into coordination vs monetization components').

omega_variable(
    data_subject_identity_lock,
    'Is the data subject''s inability to exit primarily due to material barriers (network effects, service dependency, switching costs) or to identity fusion (the subject''s online identity and social graph are constituted through the platform)?',
    'Longitudinal tracking of users post-exit: do subjects who leave platforms abandon their online identities (supporting identity_locked diagnosis) or do they actively recreate identities on alternatives (supporting trapped/constrained diagnosis based on switching costs)? Survey data on reasons for non-exit.',
    'If identity_locked: the binding mechanism is cognitive; classification from subject perspective remains snare (trapped) because structural mobility cannot be exercised. If trapped/constrained: exit is materially infeasible; different remediation strategies (interoperability, data portability) target structural barriers. Affects exit_options classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_subject_identity_lock, empirical, 'Whether data subject lock-in is structural or identity-based').

omega_variable(
    regulatory_arbitrage_sustainability,
    'Can regulatory coalitions enforce data protection standards globally, or do network effects ensure that compliant platforms lose competitiveness to less-compliant alternatives operating in permissive jurisdictions?',
    'Comparative analysis of platform behavior in high-regulation (EU, California) vs low-regulation jurisdictions; measurement of regulatory compliance cost; market share trends for compliant vs non-compliant platforms; enforcement gap analysis for cross-border data flows.',
    'If arbitrage persists: scaffold sunset is aspirational — platforms can remain extracted by operating in permissive zones. Regulatory perspective fails to deliver real constraints on monetization. If enforcement effective: scaffold transforms into rope (coordination mechanism of data governance becomes stable). Affects sustainability of sunset clause.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_arbitrage_sustainability, empirical, 'Whether regulatory standards can be enforced globally or are subject to arbitrage').

omega_variable(
    alternative_governance_viability,
    'Can decentralized, user-owned, or cooperative data governance models (blockchain platforms, cooperative social networks, user-controlled data vaults) achieve functional equivalence to centralized platforms at scale?',
    'Technical analysis of decentralized architecture requirements; economic analysis of cost structures for decentralized vs centralized platforms; user behavior comparison on decentralized vs centralized services; adoption trajectories for decentralized alternatives.',
    'If viable: competing platform perspective strengthens — network effects are not permanently lock-in. If unviable: lock-in is structural — tangled rope classification for competitors is correct, and data subject''s exit options remain trapped regardless of regulatory intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_governance_viability, empirical, 'Whether decentralized data governance can compete with centralized platforms').

omega_variable(
    suppression_mechanism_layers,
    'Is suppression of data subject exit primarily structural (absence of alternatives, switching costs, network effects) or institutional (deliberate obfuscation, dark patterns, contractual lock-in)?',
    'Design intervention testing: create transparent, frictionless data export and portability tools; measure actual exit rates and switching behavior. Audit privacy policy language for obfuscation. Analyze UI/UX patterns for dark patterns (choice architecture deliberately making exit costly).',
    'If structural: even perfect transparency won''t enable meaningful exit; suppression=0.68 is accurate. If institutional: reducing obfuscation and enabling portability could lower suppression to 0.40–0.50. Affects Tangled Rope vs Snare boundary for data subjects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_layers, empirical, 'Decomposition of suppression into structural vs institutional components').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(data_monetization_asymmetry, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(datamon_tr_t0, data_monetization_asymmetry, theater_ratio, 0, 0.3).
narrative_ontology:measurement(datamon_tr_t5, data_monetization_asymmetry, theater_ratio, 5, 0.42).
narrative_ontology:measurement(datamon_tr_t10, data_monetization_asymmetry, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(datamon_be_t0, data_monetization_asymmetry, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(datamon_be_t5, data_monetization_asymmetry, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(datamon_be_t10, data_monetization_asymmetry, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(data_monetization_asymmetry, resource_allocation).
narrative_ontology:affects_constraint(data_monetization_asymmetry, algorithmic_opacity_barrier).
narrative_ontology:affects_constraint(data_monetization_asymmetry, network_effect_lock_in).
narrative_ontology:affects_constraint(data_monetization_asymmetry, privacy_governance_capture).
narrative_ontology:affects_constraint(data_monetization_asymmetry, attention_economy_extraction).

% DUAL FORMULATION NOTE:
% Data monetization asymmetry is a constraint family with multiple decomposable claims: (1) data_value_capture_gap — economic asymmetry in who captures value from data (ε≈0.50, snare from user perspective); (2) data_access_barrier — technological barrier to data portability and competitive parity (ε≈0.35, rope from technical perspective); (3) algorithmic_suppression_mechanism — extraction through opaque algorithmic decision-making that advantages platforms (ε≈0.62, tangled rope). These stories are linked because data monetization relies on all three mechanisms working together. The primary story treats data monetization as a unified tangled rope; upstream stories decompose specific failure modes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(data_monetization_asymmetry, institutional, 0.2).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
