% ============================================================================
% CONSTRAINT STORY: eidas_digital_identity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_eidas_digital_identity, []).

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
 *   constraint_id: eidas_digital_identity
 *   human_readable: eIDAS Digital Identity Regulation and Sovereign Control
 *   domain: governance/digital_infrastructure/privacy
 *
 * SUMMARY:
 *   The eIDAS (electronic IDentification, Authentication and trust Services)
 *   regulation represents the European Union's attempt to create a unified
 *   digital identity framework enabling cross-border recognition of national
 *   digital identities while preserving member state sovereignty over
 *   identity issuance. The regulation functions simultaneously as a
 *   coordination mechanism (solving mutual recognition problems) and an
 *   extraction mechanism (gatekeeping alternative identity ecosystems and
 *   concentrating control in state-authorized providers). The constraint
 *   exhibits high suppression (0.62) because the regulatory framework legally
 *   mandates eIDAS credential acceptance for essential services while
 *   suppressing decentralized and alternative identity systems through
 *   compliance barriers, liability frameworks, and wallet standardization.
 *   Theater has increased over the regulatory implementation period (0.35 to
 *   0.48) as compliance reporting and certification processes have
 *   proliferated without corresponding transformation in actual digital
 *   identity adoption. Extractiveness has risen (0.42 to 0.58) as the
 *   regulatory framework has solidified into a gatekeeping mechanism
 *   protecting state and incumbent provider monopolies.
 *
 * KEY AGENTS:
 *   - EU Citizens: Primary victims (powerless/trapped) — cannot opt out of eIDAS framework for essential services; alternative identity systems suppressed by regulation
 *   - Member States (eID Authorities): Primary beneficiary (institutional/arbitrage) — retain sovereignty over identity criteria; gain automatic EU-wide recognition; arbitrage between regulatory compliance and implementation timeline
 *   - Alternative Identity Ecosystems (SSI, DIDs, Blockchain): Secondary victims (moderate/constrained) — face regulatory barriers, liability uncertainty, compliance costs; also benefit from standardization enabling interoperability
 *   - Tech Platforms (Large Digital Service Providers): Secondary victim (powerful/constrained) — required to accept eIDAS credentials, reducing proprietary identity control; benefit from credential trust layer
 *   - Identity Service Providers (Private Companies): Secondary beneficiary (institutional/arbitrage) — gain certified provider status enabling market access; constrained by eIDAS standards
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing regulatory framework as inevitable solution to identity governance problems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eidas_digital_identity, 0.58).
domain_priors:suppression_score(eidas_digital_identity, 0.62).
domain_priors:theater_ratio(eidas_digital_identity, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eidas_digital_identity, extractiveness, 0.58).
narrative_ontology:constraint_metric(eidas_digital_identity, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(eidas_digital_identity, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(eidas_digital_identity, tangled_rope).
narrative_ontology:human_readable(eidas_digital_identity, "eIDAS Digital Identity Regulation and Sovereign Control").
narrative_ontology:topic_domain(eidas_digital_identity, "governance/digital_infrastructure/privacy").

domain_priors:requires_active_enforcement(eidas_digital_identity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(eidas_digital_identity, eu_member_states).
narrative_ontology:constraint_beneficiary(eidas_digital_identity, identity_service_providers).
narrative_ontology:constraint_victim(eidas_digital_identity, citizens_cross_border_mobility).
narrative_ontology:constraint_victim(eidas_digital_identity, alternative_identity_ecosystems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED CITIZEN (SNARE) — EU citizens cannot opt out of eIDAS framework for essential services (tax filing, banking, legal verification). Alternative identity ecosystems (decentralized identity, blockchain-based credentials) face regulatory suppression. High suppression (0.62): legal mandate, infrastructure investment by states, interoperability gatekeeping. No viable exit for essential transactions. Maximum extraction experienced.
constraint_indexing:constraint_classification(eidas_digital_identity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: ALTERNATIVE IDENTITY ECOSYSTEM (TANGLED ROPE) — Blockchain-based SSI (Self-Sovereign Identity) projects, decentralized identity platforms face regulatory constraints but also benefit from eIDAS interoperability standards. Constrained by: compliance costs, legal uncertainty, pressure to conform to eIDAS trust framework. Benefit from: standardization enabling cross-border recognition, infrastructure investment. Moderate extraction with genuine coordination function — eIDAS creates specification layer that reduces fragmentation.
constraint_indexing:constraint_classification(eidas_digital_identity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEMBER STATE AUTHORITY (ROPE) — National eID issuing authorities (governmental actors) experience eIDAS as pure coordination. The regulation solves the collective action problem of cross-border mutual recognition without imposing extraction. Member states retain sovereignty over identity criteria and issuance; eIDAS standardizes notification and trust framework. Net beneficiary through arbitrage: can issue identity credentials that gain automatic EU-wide acceptance.
constraint_indexing:constraint_classification(eidas_digital_identity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: REGULATED TECH PLATFORM (TANGLED ROPE) — Large digital service providers (Meta, Google, Amazon) are required to accept eIDAS credentials for identity verification, reducing their ability to maintain proprietary identity ecosystems. Constrained by: regulatory mandate to interoperate, reduced data collection for identity verification. Benefit from: credential trust layer enables higher-friction services, reduces fraud liability. Asymmetric extraction — platforms lose identity data control but gain verification reliability.
constraint_indexing:constraint_classification(eidas_digital_identity, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: eIDAS REGULATORY FRAMEWORK AS PITON (PITON) — eIDAS 2.0 implementation largely consists of compliance reporting, certification processes, and interoperability testing that produce theater without functional digital identity transformation. Theater ratio (0.48) reflects performative compliance: many EU citizens still use paper identification for essential transactions. The regulation persists through institutional inertia and sovereignty preservation (states maintain issuing authority) rather than because a genuine alternative identity infrastructure has displaced legacy systems.
constraint_indexing:constraint_classification(eidas_digital_identity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, digital identity governance faces immutable structural constraints: any system that verifies identity at scale must make trade-offs between privacy, security, and usability; any cross-border system must solve the mutual recognition problem; any digital credential system faces the binding problem (proving the digital credential belongs to the person presenting it). These constraints appear as natural law. However, structural analysis reveals eIDAS as a contingent institutional response to these problems, not the inevitable solution — the framework reflects European regulatory preferences and state sovereignty maintenance, not immutable necessity.
constraint_indexing:constraint_classification(eidas_digital_identity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eidas_digital_identity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(eidas_digital_identity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(eidas_digital_identity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(eidas_digital_identity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(eidas_digital_identity, TR),
    TR >= 0.70.

:- end_tests(eidas_digital_identity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The regulation extracts from citizens through mandatory adoption and from alternative identity ecosystems through competitive suppression. However, extraction is not severe (would justify ≥0.70) because the framework delivers genuine coordination benefits (cross-border mutual recognition) and member states retain authority, reducing perceived extraction from their perspective. The value reflects that extractiveness is asymmetric: high from trapped citizens' perspective, moderate from powerful tech platforms, low from member state authorities. Suppression (0.62): Moderate-high. Legal mandate for acceptance, infrastructure investment in eIDAS networks, regulatory barriers to alternative systems, and liability frameworks that disfavor non-governmental issuers create significant suppression. But suppression is not total (0.70+) because paper identification remains acceptable for many transactions, and some alternative systems (self-hosted credentials) remain technically feasible if legally risky. Theater ratio (0.48): Moderate. eIDAS implementation includes substantial performative elements: certification processes, interoperability testing, compliance reporting that do not translate to citizen adoption. However, the framework has genuine coordination function (standardization enabling cross-border service), and alternative identity systems have meaningful technical barriers that eIDAS addresses, so theater is not high (>0.70).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals that eIDAS is experienced as pure extraction (Snare) by trapped citizens, as mixed coordination-extraction (Tangled Rope) by moderate alternative identity providers and powerful tech platforms, as pure coordination (Rope) by member state authorities, and as theater (Piton) from civilizational view where the regulatory framework persists through inertia despite low actual digital identity transformation. The gap between the member state authority perspective (Rope — pure coordination) and the trapped citizen perspective (Snare — pure extraction) is maximal: the same regulatory framework solves a coordination problem for states while imposing extraction on citizens. This gap is diagnostic: it reveals that the coordination function is asymmetrically distributed. The member states are genuinely solving a mutual recognition problem. The citizens are being forced into a single identity infrastructure with suppressed alternatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from structural position relative to extraction flow. Member states have institutional power and arbitrage exit (can issue identity credentials that gain automatic EU-wide acceptance without cost): d ≈ 0.05 (beneficiary). Citizens have powerless status and trapped exit (cannot opt out of essential transactions): d ≈ 0.95 (full target). Alternative identity ecosystems have moderate power and constrained exit (face regulatory barriers but technically feasible): d ≈ 0.70 (victim with some agency). Tech platforms have powerful status but constrained exit within digital services market (must accept eIDAS credentials): d ≈ 0.55 (mixed). The derived directionality values map to experienced extractiveness (chi) through the sigmoid f(d). Citizens experience maximum effective extraction; member states experience minimum or negative extraction (subsidy); moderate actors experience moderate extraction proportional to their constraint-specific barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that eIDAS is genuinely a tangled rope: it solves a real coordination problem (member state mutual recognition of digital identities) while simultaneously enabling extraction (gatekeeping alternative ecosystems, centralizing control in state-authorized providers). The tension between types is not ambiguity in the classification — it is structural reality. Member states see Rope because the coordination function is real for them. Citizens see Snare because they cannot exit and extraction is asymmetric. The analytical observer risks seeing Mountain (naturalizing the framework as inherent to digital identity governance) but structural analysis reveals this as false summit: the regulatory choices (state monopoly on issuance, suppression of alternatives) are contingent, not inevitable. The mandatrophy prevents mislabeling the framework as pure coordination (Rope) by clarifying that coordination benefits are asymmetrically captured: member states and incumbent providers benefit; citizens and alternative ecosystems bear costs. Conversely, it prevents mislabeling as pure extraction (Snare) by acknowledging the genuine cross-border coordination problem and the real service benefits (though asymmetrically distributed). Tangled Rope captures this dual structure: active enforcement of standards, genuine coordination function, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    privacy_extraction_mechanism,
    'Does eIDAS framework extraction operate through data collection (privacy cost) or through regulatory gatekeeping (competitive cost)?',
    'Comparative analysis of personal data flows: eIDAS credential issuance and verification vs alternative identity systems. Audit of member state data collection practices under eIDAS vs pre-eIDAS.',
    'If primarily data extraction: classify as information_asymmetry snare. If primarily regulatory gatekeeping: classify as market_structure tangled_rope. Different extraction mechanisms suggest different victim prioritization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(privacy_extraction_mechanism, empirical, 'Whether extraction operates through data collection or regulatory gatekeeping').

omega_variable(
    decentralized_identity_suppression,
    'Is regulatory suppression of decentralized identity systems (blockchain SSI, DIDs) structural necessity or contingent policy choice?',
    'Analysis of eIDAS 2.0 wallet requirements, trust framework gatekeeping, liability frameworks for non-governmental issuers. Comparison with jurisdictions permitting competitive identity ecosystems.',
    'If structural necessity: suppression reflects binding problem of cross-border recognition. If contingent policy: suppression is extraction mechanism protecting member state monopoly on identity authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(decentralized_identity_suppression, conceptual, 'Whether decentralized identity suppression is structural or contingent').

omega_variable(
    cross_border_mobility_benefit,
    'Does eIDAS coordination function actually enable cross-border services that were impossible pre-regulation, or does it primarily formalize existing practice?',
    'Longitudinal analysis of cross-border service adoption rates: banking, tax filing, healthcare pre-eIDAS vs post-eIDAS. Counterfactual: would market coordination have achieved similar interoperability without regulation?',
    'If genuine enabler: coordination function is real, tangled_rope classification appropriate. If formalization of existing practice: coordination benefit is minimal, extraction function dominates, reclassify toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_border_mobility_benefit, empirical, 'Whether eIDAS enables new cross-border services or formalizes existing practice').

omega_variable(
    sovereign_dignity_vs_extraction,
    'Does member state retention of eID issuance authority represent legitimate sovereignty preservation or extractive gatekeeping mechanism protecting national monopoly?',
    'Comparative institutional analysis: do other policy domains (banking regulation, telecommunications) permit competing infrastructure, or does eIDAS uniquely preserve state monopoly on identity?',
    'If sovereignty preservation: member states are not victims, rope classification appropriate. If extractive monopoly: member states extract rents through regulatory gatekeeping, beneficiary status should be re-evaluated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereign_dignity_vs_extraction, preference, 'Whether state eID monopoly reflects legitimate sovereignty or extractive gatekeeping').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(eidas_digital_identity, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(eidas_tr_t0, eidas_digital_identity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(eidas_tr_t3, eidas_digital_identity, theater_ratio, 3, 0.42).
narrative_ontology:measurement(eidas_tr_t6, eidas_digital_identity, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(eidas_be_t0, eidas_digital_identity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(eidas_be_t3, eidas_digital_identity, base_extractiveness, 3, 0.5).
narrative_ontology:measurement(eidas_be_t6, eidas_digital_identity, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(eidas_digital_identity, enforcement_mechanism).
narrative_ontology:affects_constraint(eidas_digital_identity, decentralized_identity_gatekeeping).
narrative_ontology:affects_constraint(eidas_digital_identity, eu_digital_sovereignty_framework).

% DUAL FORMULATION NOTE:
% eIDAS digital identity regulation has distinct structural aspects: (1) cross-border mutual recognition coordination (Rope-primary), (2) alternative identity ecosystem suppression (Snare-primary), (3) state monopoly on eID issuance (extraction via gatekeeping). These could decompose into three separate constraint stories with ε values: mutual_recognition_coordination (ε≈0.15, Rope), alternative_identity_suppression (ε≈0.65, Snare), state_eID_monopoly (ε≈0.45, Tangled Rope). The present story unifies them because they are inseparable in the regulatory implementation: the coordination mechanism IS the extraction mechanism — standardization gatekeeps alternatives; mutual recognition centralizes authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(eidas_digital_identity, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
