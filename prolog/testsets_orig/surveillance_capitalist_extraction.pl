% ============================================================================
% CONSTRAINT STORY: surveillance_capitalist_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_surveillance_capitalist_extraction, []).

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
 *   constraint_id: surveillance_capitalist_extraction
 *   human_readable: Surveillance Capitalist Data Extraction and Behavioral Control
 *   domain: digital_political_economy/platform_governance
 *
 * SUMMARY:
 *   Surveillance capitalism refers to the business model of capturing
 *   behavioral data from platform users, modeling their preferences and
 *   vulnerabilities, and selling access to behavioral predictions to
 *   advertisers. The constraint exhibits characteristics of pure extraction
 *   (snare) from the user perspective, mixed coordination-extraction (tangled
 *   rope) from the platform perspective, and coordination (rope) from the
 *   advertiser perspective. The extractiveness has increased from 0.35
 *   (2009-2014, early social media era with limited tracking) to 0.68
 *   (present), driven by increasing sophistication of inference models,
 *   expansion to non-users through data brokers, and proliferation of
 *   collection points. Theater ratio has also increased (0.35 to 0.65) as
 *   regulatory compliance mechanisms (consent forms, privacy policies) have
 *   become performative rather than functional. The constraint is a snare
 *   because users face structural entrapment: all major communication
 *   platforms employ identical tracking mechanisms, making exit effectively
 *   impossible; users cannot coordinate to demand better terms; suppression
 *   operates through informational asymmetry (hidden tracking, opaque
 *   algorithms) and identity lock-in (social identity fused with platform
 *   participation). Non-users are trapped in an inference mechanism they
 *   cannot perceive or contest. The platform companies experience this as
 *   tangled rope: they solve a genuine market problem (connecting advertisers
 *   to relevant audiences) while extracting behavioral surplus without user
 *   consent. Behavioral advertisers experience pure coordination (rope) —
 *   they benefit from efficient targeting without experiencing coercion.
 *   Privacy regulators and coalitions see a solvable problem with a sunset
 *   (scaffold) — privacy-preserving architecture and interoperability
 *   standards are emerging alternatives. The mechanism exhibits active
 *   enforcement (algorithmic lock-in, legal immunity for platforms) and high
 *   suppression (users cannot see what data is collected, cannot delete
 *   profiles, cannot opt out of inference).
 *
 * KEY AGENTS:
 *   - Platform Users: Primary victims (powerless/trapped) — structurally locked in by network effects, social pressure, workplace integration; experience invisible tracking and behavioral nudging; no functional exit
 *   - Non-Users Targeted by Inference: Secondary victims (powerless/identity_locked) — modeled and targeted without awareness or consent; structurally invisible to themselves as targets; zero agency; maximum suppression
 *   - Attention Merchants (Platform Companies): Primary beneficiaries (institutional/arbitrage) — capture behavioral surplus; solve market coordination problem; experience constraint as beneficial lock-in mechanism; benefit from informational asymmetry
 *   - Behavioral Advertisers: Secondary beneficiaries (powerful/arbitrage) — access fine-grained targeting; reduce advertising waste; experience constraint as pure coordination
 *   - Data Brokers: Tertiary beneficiaries (institutional/arbitrage) — aggregate and sell inferences about non-users; operate in regulatory shadow; maximize extraction with minimal visibility
 *   - Regulatory Frameworks: Institutional actor (institutional/constrained) — GDPR/CCPA create consent requirements that have become theatrical; enforce through dark pattern policies; see own mechanisms as degraded
 *   - Privacy Rights Coalition: Organized challenger (organized/constrained) — advocating for data portability, interoperability, federated standards; see structural exit path through technical and regulatory pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(surveillance_capitalist_extraction, 0.68).
domain_priors:suppression_score(surveillance_capitalist_extraction, 0.72).
domain_priors:theater_ratio(surveillance_capitalist_extraction, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(surveillance_capitalist_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(surveillance_capitalist_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(surveillance_capitalist_extraction, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(surveillance_capitalist_extraction, snare).
narrative_ontology:human_readable(surveillance_capitalist_extraction, "Surveillance Capitalist Data Extraction and Behavioral Control").
narrative_ontology:topic_domain(surveillance_capitalist_extraction, "digital_political_economy/platform_governance").

domain_priors:requires_active_enforcement(surveillance_capitalist_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(surveillance_capitalist_extraction, attention_merchants).
narrative_ontology:constraint_beneficiary(surveillance_capitalist_extraction, behavioral_advertisers).
narrative_ontology:constraint_beneficiary(surveillance_capitalist_extraction, data_brokers).
narrative_ontology:constraint_victim(surveillance_capitalist_extraction, platform_users).
narrative_ontology:constraint_victim(surveillance_capitalist_extraction, non_users_targeted_by_inference).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLATFORM USER (SNARE) — User is behaviorally trapped in digital environment with no functional alternative. Structural mobility exists (switching phones, platforms) but is prohibitively costly (social isolation, workplace incompatibility, identity dissolution). Invisible tracking and behavioral nudging extract behavioral surplus with no consent or compensation. Suppression operates through informational asymmetry, device lock-in, and social pressure. User perceives the constraint as immutable because all peers are trapped identically.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-USER TARGETED BY INFERENCE (SNARE) — Individual who does not use platforms but is modeled and targeted through inference based on network effects and data broker aggregation. No login, no consent, no awareness of modeling. Exit is structurally impossible — cannot refuse to exist in others' social graphs or refuse to be inferred. Maximum extractiveness with zero agency. Suppression is total because the target has no access to information about how they are modeled or which predictions control their opportunities.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: ATTENTION MERCHANT (TANGLED ROPE) — Platform company experiences constraint as coordination problem (aggregating user attention for advertiser access) with significant extraction benefit (monopolistic pricing power, behavioral surplus capture). Active enforcement required to maintain lock-in (API restrictions, algorithm opacity, legal immunity). Genuine coordination function: connecting advertisers to audiences solves a market problem. Asymmetric extraction: users bear costs of manipulation; merchants capture rents. Net beneficiary.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BEHAVIORAL ADVERTISER (ROPE) — Experiences constraint as pure coordination mechanism: access to granular behavioral data enables efficient targeting, reducing waste in advertising spend. Cooperative arrangement with platform for mutual benefit. No coercion experienced by this agent; exit is available but unattractive (unmodeled audiences are expensive to reach). Extraction flows from users/merchants to advertisers; advertisers see the mechanism as beneficial information flow.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — Privacy regulations (GDPR, CCPA, DMA) and consent requirements are largely theatrical: dark patterns in consent flows, vague privacy policies, technical complexity ensure near-universal acceptance. Users click 'accept' without understanding terms. Theater ratio reflects that regulatory compliance is performed (checkboxes, disclosures) but functional suppression continues unchanged. Regulations persist through institutional momentum and lobbying resistance but have degraded into performative compliance rituals.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: PRIVACY RIGHTS COALITION (SCAFFOLD) — Organized actors (privacy advocates, interoperability consortiums, decentralized-web developers) see surveillance extraction as a solvable coordination problem with a sunset. Data portability rights, interoperability standards, and federated alternatives are structural solutions that bypass the lock-in mechanism. High suppression is tolerated because the coalition has a visible exit pathway (regulatory pressure, technical alternatives) and a timeline (10-15 years for privacy-preserving architecture maturation). Classified as Scaffold because the extraction mechanism has declining force as alternatives mature.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, surveillance capitalism has both genuine coordination function (connecting advertisers to audiences at scale solves a real market problem) and asymmetric extraction (behavioral surplus capture without consent generates rents that accrue to merchants, not users). The mechanism is not purely extractive (rope) because it solves a coordination problem; not purely coordinated (snare) because extraction is built into the design. Active enforcement and suppression are structural requirements. Classification is Tangled Rope at the highest epistemic level.
constraint_indexing:constraint_classification(surveillance_capitalist_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(surveillance_capitalist_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(surveillance_capitalist_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(surveillance_capitalist_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(surveillance_capitalist_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(surveillance_capitalist_extraction, TR),
    TR >= 0.70.

:- end_tests(surveillance_capitalist_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Platform companies extract behavioral surplus without user compensation or meaningful consent. The extraction flow runs from users to merchants asymmetrically. Measurement trajectory (0.35→0.68) shows acceleration driven by inference model sophistication and data broker expansion. The value reflects high but not total extraction (not 0.80+) because some users benefit from personalization and some users are compensated through free services — but the benefit-cost asymmetry is severe. Suppression (0.72): High. Users cannot see what data is collected, cannot effectively opt out, cannot switch platforms without social cost, and are subjected to algorithmic nudging they don't perceive. Informational asymmetry is structural: platforms know users' behavioral patterns better than users know themselves. Non-users are suppressed completely — they have no knowledge of inference mechanisms targeting them. Theater ratio (0.65): Moderate-high. Consent flows and privacy policies perform compliance without functional privacy protection; dark patterns ensure near-universal acceptance of terms; regulatory requirements become theater for demonstrating 'privacy-consciousness' while extraction continues. Theater ratio increase (0.35→0.65) reflects proliferation of compliance performance mechanisms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The powerless user sees snare (trapped, exploited, no alternative). The institutional platform sees tangled rope (genuine coordination problem + asymmetric benefit). The powerful advertiser sees rope (pure coordination, no coercion). The regulatory framework sees piton (performative compliance, degraded function). The privacy coalition sees scaffold (solvable with technical/regulatory sunset). The analytical observer (civilizational scope) sees tangled rope (both coordination and extraction are real structural features). The gap reflects that users and platforms have fundamentally opposed interests: users want privacy and agency; platforms want behavioral predictability and control. The gap is NOT resolvable by better metrics — it reflects real structural antagonism. Users experience extraction; platforms experience coordination; both are measuring the same mechanism accurately from their position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position in the extraction flow. Platform users as victims with trapped exit options generate high d (~0.95), experiencing maximum f(d)→1.42, yielding high χ for low power atoms. Non-users via inference have d≈1.0 (structurally invisible targets) with maximum f(d). Platform companies as beneficiaries with arbitrage exit options have d≈0.05-0.15, experiencing negative or near-zero f(d), yielding low/negative χ for institutional power — they experience the constraint as beneficial coordination, not extraction. Behavioral advertisers as beneficiaries have similar directionality (d≈0.10), seeing pure coordination. Regulatory frameworks with constrained exit (cannot eliminate consent requirement without political cost) have d≈0.60-0.70, experiencing moderate extraction-as-constraint. Privacy coalition with constrained but visible exit path have d≈0.50-0.60. The analytical observer has canonical d≈0.73 (analytical power atom).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint's extractiveness (0.68) and suppression (0.72) clearly map to snare classification from the user perspective. The challenge to mandatrophy is that the platform perspective (institutional/arbitrage/immediate) classifies the same structure as tangled rope — both coordination AND extraction. This is resolved by recognizing that the tuple (P,T,E,S) determines not whether extraction exists but how much extraction THIS AGENT EXPERIENCES. The platform company genuinely solves a coordination problem (connecting advertisers to audiences); the extraction is real but flows TOWARD this agent, not away. The snare and tangled rope classifications are simultaneous truths from different positions in the constraint structure. The mandatrophy resolves because the framework explicitly accommodates perspectival pluralism: the constraint IS both snare (for users) and tangled rope (for platforms). No single type 'wins' — the presheaf of perspectives over the observation site IS the answer. The false summit risk comes from the analytical observer trying to find a single 'true' type that applies universally — the constraint is not mountain (immutable) because it is structurally contingent on platforms' technical and legal immunity; it is not rope (pure coordination) because extraction is asymmetric and non-consensual; it IS the tangled rope/snare hybrid precisely because it requires active enforcement (legal immunity, algorithmic lock-in) AND produces asymmetric extraction AND solves a coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_surplus_fungibility,
    'Is behavioral surplus a fungible economic resource that can be compensated, or is it ontologically incommensurable with individual autonomy?',
    'Test whether users offered direct compensation for behavioral data show reduced extraction experience; examine whether payment mechanisms change the structural power asymmetry',
    'If fungible and compensable: constraint is Tangled Rope with a pricing problem (Rope if users were paid). If incommensurable: constraint remains Snare because no compensation erases the modeling relationship.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_surplus_fungibility, conceptual, 'Whether behavioral surplus is economically fungible or autonomy-incommensurable').

omega_variable(
    inference_targeting_feasibility,
    'Can non-user targeting via inference ever be regulated effectively, or is the inference mechanism structurally invisible?',
    'Longitudinal audit of data broker inference accuracy and targeting impact on non-users; comparison of visibility/contestability for direct targeting vs inference targeting',
    'If regulable: regulatory frameworks could extend to non-user protection. If structurally invisible: non-user victims remain in maximum-extraction trap indefinitely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inference_targeting_feasibility, empirical, 'Whether inference-based targeting can be regulated or remains structurally opaque').

omega_variable(
    interoperability_viability,
    'Can federated/decentralized platforms actually compete with network-effect-driven incumbents, or is the lock-in mathematically inescapable?',
    'Historical analysis of network-effect lock-in escapes (multi-homing, protocol switches); modeling of critical mass thresholds for alternative platforms',
    'If viable: scaffold sunset is real (privacy-tech can emerge). If inescapable: sunset is aspirational and users remain trapped indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_viability, empirical, 'Whether federated platforms can escape network-effect incumbents').

omega_variable(
    consent_dark_patterns_legal_status,
    'Are dark pattern consent flows legal violations or regulatory-compliant optimization?',
    'Regulatory enforcement actions; court rulings on UI design standards; correlation between GDPR/CCPA enforcement and consent flow improvement',
    'If violations: regulatory framework is failing to enforce, and theater ratio increases. If compliant: regulations are structurally insufficient for user protection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_dark_patterns_legal_status, empirical, 'Whether dark patterns constitute regulatory violations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(surveillance_capitalist_extraction, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, surveillance_capitalist_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(surv_tr_t5, surveillance_capitalist_extraction, theater_ratio, 5, 0.5).
narrative_ontology:measurement(surv_tr_t10, surveillance_capitalist_extraction, theater_ratio, 10, 0.65).
narrative_ontology:measurement(surv_tr_t15, surveillance_capitalist_extraction, theater_ratio, 15, 0.72).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, surveillance_capitalist_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(surv_be_t5, surveillance_capitalist_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(surv_be_t10, surveillance_capitalist_extraction, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(surv_be_t15, surveillance_capitalist_extraction, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(surveillance_capitalist_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(surveillance_capitalist_extraction, 0.18).
narrative_ontology:affects_constraint(surveillance_capitalist_extraction, attention_economy_winner_take_all).
narrative_ontology:affects_constraint(surveillance_capitalist_extraction, algorithmic_recommendation_lock_in).
narrative_ontology:affects_constraint(surveillance_capitalist_extraction, data_broker_shadow_economy).
narrative_ontology:affects_constraint(surveillance_capitalist_extraction, regulatory_capture_platform_immunity).

% DUAL FORMULATION NOTE:
% Surveillance capitalism decomposes into multiple structural constraints: (1) base data extraction (behavioral modeling), (2) attention extraction (algorithmic feeds), (3) inference targeting of non-users, (4) regulatory capture preventing remediation. This story models the integrated constraint; the network links document structural dependencies where failures in one cascade to others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(surveillance_capitalist_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
