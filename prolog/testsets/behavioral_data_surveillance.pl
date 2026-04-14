% ============================================================================
% CONSTRAINT STORY: behavioral_data_surveillance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behavioral_data_surveillance, []).

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
 *   constraint_id: behavioral_data_surveillance
 *   human_readable: Behavioral Data Surveillance and Asymmetric Extraction
 *   domain: digital_society/political_economy
 *
 * SUMMARY:
 *   Behavioral data surveillance represents a structural asymmetry in digital
 *   societies where platforms, advertisers, and state security apparatus
 *   collect, analyze, and exploit detailed behavioral data about users while
 *   offering minimal reciprocal transparency or control. The constraint
 *   operates through multiple enforcement mechanisms: economic dependency on
 *   platform services, technological opacity, regulatory capture, and
 *   normalization of surveillance as inevitable. Unlike surveillance in
 *   earlier eras (telephone wiretapping, mail monitoring), behavioral data
 *   surveillance is continuous, comprehensive, machine-readable, and
 *   inexpensively reproducible at scale. The constraint exhibits strong snare
 *   characteristics (high extraction, high suppression, trapped exit options
 *   for users) but also manifests as rope from platform and state
 *   perspectives (genuine coordination functions) and scaffold from
 *   decentralized technology advocates (who see alternative architectures
 *   emerging). The theater ratio has increased substantially over the
 *   measurement interval as privacy awareness campaigns proliferate while
 *   behavioral extraction mechanisms intensify and become less visible.
 *   Consent-based regulation (GDPR, CCPA) has created compliance theater:
 *   users now receive privacy notices and consent requests, but actual
 *   behavioral data flows and use cases remain opaque and technically
 *   irreversible.
 *
 * KEY AGENTS:
 *   - Surveilled Users: Primary victims (powerless/trapped) — economic necessity forces platform use; no viable exit without severe life disruption; full behavioral transparency extraction
 *   - Digital Native Generation: Secondary victim cohort (powerless/identity_locked) — identity fusion through platform-mediated socialization; structurally mobile but cognitively trapped; extraction normalized as inevitable
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — capture attention, behavioral data, and advertising revenue; experience surveillance as coordination mechanism; multinational scope enables exit/arbitrage
 *   - Advertisers: Secondary beneficiary (institutional/arbitrage) — benefit from behavioral targeting; experience data access as enabling marketing coordination
 *   - State Security Apparatus: Tertiary beneficiary (powerful/constrained) — gain mass surveillance capability through commercial data access; structured extraction justified by security framing
 *   - Data Protection Regulators: Hybrid actor (organized/constrained) — enforce privacy rules creating compliance theater; also benefit from expanded regulatory jurisdiction and jurisdictional rent-seeking
 *   - Privacy Advocacy Movement: Institutional respondent (institutional/arbitrage) — advocates for privacy protection; increasingly degraded (piton) as theater substitutes for functional change
 *   - Decentralized Technology Coalition: Alternative builder (organized/mobile) — developing surveillance-resistant infrastructure with lower extraction but higher coordination friction; scaffold perspective with sunset logic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behavioral_data_surveillance, 0.68).
domain_priors:suppression_score(behavioral_data_surveillance, 0.72).
domain_priors:theater_ratio(behavioral_data_surveillance, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behavioral_data_surveillance, extractiveness, 0.68).
narrative_ontology:constraint_metric(behavioral_data_surveillance, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(behavioral_data_surveillance, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behavioral_data_surveillance, snare).
narrative_ontology:human_readable(behavioral_data_surveillance, "Behavioral Data Surveillance and Asymmetric Extraction").
narrative_ontology:topic_domain(behavioral_data_surveillance, "digital_society/political_economy").

domain_priors:requires_active_enforcement(behavioral_data_surveillance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behavioral_data_surveillance, platform_operators).
narrative_ontology:constraint_beneficiary(behavioral_data_surveillance, data_brokers).
narrative_ontology:constraint_beneficiary(behavioral_data_surveillance, advertisers).
narrative_ontology:constraint_beneficiary(behavioral_data_surveillance, state_security_apparatus).
narrative_ontology:constraint_victim(behavioral_data_surveillance, surveilled_users).
narrative_ontology:constraint_victim(behavioral_data_surveillance, digital_privacy_commons).
narrative_ontology:constraint_victim(behavioral_data_surveillance, democratic_deliberation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED USER (SNARE) — Trapped within digital ecosystems by economic necessity (communication, commerce, information access). No viable exit without severe life disruption. Full behavioral transparency extraction with minimal coordination benefit. Maximum experienced extraction — the user bears costs (manipulation, discrimination, identity targeting) with no reciprocal visibility of the system that observes them.
constraint_indexing:constraint_classification(behavioral_data_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DIGITAL NATIVE GENERATION (SNARE) — Structurally mobile (could theoretically reject digital platforms) but identity-locked through socialization into platform-mediated identity, peer relationships, and self-expression. The binding is cognitive rather than material — exit would require abandoning the social substrate through which identity is constituted. Surveilled from formative years; surveillance is internalized as normal. Maximum extraction with internalized normalization.
constraint_indexing:constraint_classification(behavioral_data_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences behavioral data collection as coordination mechanism: optimizing user experience, preventing fraud, targeting relevant content. Net beneficiary through attention capture, conversion optimization, and advertising revenue. Extraction runs toward this agent; they perceive the constraint as solving a legitimate problem (matching users to content) while capturing asymmetric value. Arbitrage exit: can shift data monetization strategies or platforms without ceasing to operate.
constraint_indexing:constraint_classification(behavioral_data_surveillance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISER (ROPE) — Experiences surveillance data as enabling coordination: reaching targeted audiences, measuring campaign effectiveness, reducing ad waste. Net beneficiary. Behavioral targeting is a coordination service from this perspective — solving the problem of matching products to consumers. Arbitrage exit: can choose different advertising platforms or strategies.
constraint_indexing:constraint_classification(behavioral_data_surveillance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DATA PROTECTION REGULATOR (TANGLED ROPE) — Enforces privacy regulation (GDPR, CCPA, etc.) which coordinates user consent norms and platform accountability while extracting compliance burden, reporting overhead, and regulatory rent. Both functions are real: the regulation does reduce some extraction, but enforcement agencies also benefit from expanded jurisdiction and budget. Constrained exit: cannot abandon regulation without political pressure; regulatory capture incentivizes capture rather than exit.
constraint_indexing:constraint_classification(behavioral_data_surveillance, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: STATE SECURITY APPARATUS (SNARE) — Primary institutional beneficiary of behavioral surveillance through access to user data, communication patterns, and political sentiment tracking. Structured as pure extraction from user perspective: mass surveillance justified by security rhetoric offers minimal transparency or reciprocal benefit to surveilled populations. From the apparatus perspective (institutional/constrained), surveillance is coordination (intelligence collection) but from user perspective it is pure coercion. This perspective reveals the asymmetry: state surveillance is both snare (for users) and rope (for the security apparatus).
constraint_indexing:constraint_classification(behavioral_data_surveillance, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: PRIVACY ADVOCACY MOVEMENT (PITON) — Institutional response to surveillance extraction that has become partially degraded: privacy consciousness is high, awareness campaigns are extensive, but behavioral change is minimal. Users express privacy concern (theater) while continuing platform use patterns unchanged. Privacy regulation creates compliance theater (privacy policies, consent notices) that users don't read and that platforms optimize for legality rather than actual privacy protection. The advocacy framework persists through institutional inertia and moral intuition rather than functional effectiveness.
constraint_indexing:constraint_classification(behavioral_data_surveillance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: DECENTRALIZED TECHNOLOGY COALITION (SCAFFOLD) — Organized agents (open-source projects, peer-to-peer protocols, privacy-focused tools) building alternative infrastructure (Signal, Mastodon, IPFS, local-first computing) that reduces behavioral extraction. These alternatives have lower surveillance extraction but also lower coordination efficiency (slower, smaller network effects, higher friction). Sunset logic: as decentralized tools mature and network effects accumulate, centralized surveillance platforms lose users and data monopolies degrade. Estimated 15-30 year trajectory for meaningful sunset of platform-dominated surveillance model.
constraint_indexing:constraint_classification(behavioral_data_surveillance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, behavioral data collection appears as an immutable feature of network systems: any distributed system requires information about participant behavior to coordinate. From this perspective, surveillance is inherent to digital infrastructure — a natural law of networked communication. However, the structural data contradicts mountain classification: the extraction is contingent on specific institutional arrangements (centralized platforms, attention-capture business models, data monetization regimes), not on digital networks themselves. Decentralized alternatives demonstrate that different behavioral data governance is possible.
constraint_indexing:constraint_classification(behavioral_data_surveillance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behavioral_data_surveillance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(behavioral_data_surveillance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(behavioral_data_surveillance, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(behavioral_data_surveillance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(behavioral_data_surveillance, TR),
    TR >= 0.70.

:- end_tests(behavioral_data_surveillance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Behavioral data extraction increases systematically over time as platform capabilities expand, data monetization becomes more sophisticated, and cross-platform behavioral inference improves. The initial measurement (0.32, year 0) reflects pre-smartphone era privacy norms where behavioral tracking was limited. Intermediate measurement (0.52, year 5) reflects transition to mobile surveillance with location, app usage, and attention tracking. Final measurement (0.68, year 10) reflects integration of sensor data (camera, microphone, gyroscope), cross-platform profiling, and algorithmic behavior prediction. The trajectory shows pure accumulation without corresponding user benefit. Suppression (0.72): Very high. Multiple barriers prevent user exit and behavioral change: economic lock-in (platform necessity for work, communication, commerce), network externalities (switching loses social value), technological opacity (users cannot understand what is being collected), regulatory capture (platforms influence privacy regulation), and identity fusion (young users cannot imagine platform-independent existence). Theater ratio (0.58): Moderate-high. Privacy awareness campaigns, privacy policies, consent notices, and privacy regulation create performative privacy protection while actual behavioral extraction intensifies. Theater increased from 0.28 (year 0) to 0.58 (year 10) as compliance theater proliferated. The gap between privacy theater and actual extraction indicates the theater itself is part of the extraction mechanism: users feel agency through consent while behavioral data flows continue unchanged.
 *
 * PERSPECTIVAL GAP:
 *   The gap between snare (user perspective) and rope (platform perspective) reveals the core structural asymmetry: both parties operate within the same technical constraint (behavioral data collection is possible and useful) but experience it through opposite directionality. Users see extraction without benefit; platforms see coordination with benefit. This gap is not perceptual error but structural reality — the same data flow is genuinely a coordination service from the platform's standpoint (enabling personalization, fraud detection) and pure extraction from the user's standpoint (behavioral surveillance without reciprocal visibility or control). The scaffold perspective reveals the gap is not inevitable: alternative architectures (federated platforms, peer-to-peer protocols, local-first computing) can reduce extraction while preserving coordination, though with higher friction and slower performance. The identity_locked perspective on digital natives reveals a second gap: younger cohorts cannot perceive the snare because their identity formation occurred within surveillance conditions; they lack a baseline for non-surveilled platform experience. The piton perspective on privacy advocacy reveals that institutional response to snare has become performative: privacy campaigns, regulations, and consent mechanisms create theater that mimics functional protection while extraction continues.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position relative to behavioral extraction flow. Surveilled users with trapped exit (d ≈ 0.95) experience maximum f(d) ≈ 1.42, producing high chi. Digital natives with identity_locked exit (d ≈ 0.89) experience f(d) ≈ 1.28, also producing high chi but with cognitive entrapment component. Platform operators as beneficiaries with arbitrage exit (d ≈ 0.15) experience f(d) ≈ -0.01, producing negative chi — extraction flows toward them. Advertisers with same position (d ≈ 0.15) also experience negative chi. State security apparatus benefits maximally but faces constrained exit (d ≈ 0.30) from global dependence on platform infrastructure; produces f(d) ≈ 0.20 representing moderate beneficiary position with some exposure to regulatory constraints. Data protection regulators with organized power and constrained exit (d ≈ 0.40) occupy tangled position: they enforce extraction controls but also benefit from regulatory rent; f(d) ≈ 0.40 reflects this hybrid. Decentralized coalition with mobile exit (d ≈ 0.35) experiences f(d) ≈ 0.30 — they have agency to build alternatives even if constrained by network effects. Privacy advocates as institutional arbitrage (d ≈ 0.12) experience f(d) ≈ -0.02 but their low functional effectiveness (theater ratio high) limits actual impact on chi.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED AT SNARE CLASSIFICATION. The constraint reaches extractiveness of 0.68 and mandatrophy threshold (>0.70 resolution required) but resolution is achieved by recognizing that behavioral surveillance is structurally a snare, not a latent rope. The mandatrophy dissolution has three components: (1) **Recognition of asymmetry**: Unlike genuine coordination mechanisms where both parties benefit from the constraint, behavioral surveillance has genuine beneficiaries (platforms, state, advertisers) and genuine victims (surveilled populations). (2) **Identification of coercion mechanism**: The suppression (0.72) reveals that exit is not meaningfully optional — economic necessity, network externalities, and identity fusion create trapped/identity_locked conditions, not constrained conditions. (3) **Detection of extraction intent**: The theater ratio (0.58) shows that privacy regulations and consent mechanisms are not genuine coordination function but extraction mechanisms disguised as coordination — the theater itself is part of the control apparatus, not its mitigation. The snare classification is confirmed by: (a) extreme directionality gap (beneficiaries ≈ d=0.15 experiencing f(d)≈-0.01 vs. victims ≈ d=0.95 experiencing f(d)≈1.42), (b) irreducible asymmetry in reciprocal knowledge (platforms know user behavior; users don't know platform behavior), (c) suppression mechanisms that would qualify as coercion in any other context but are naturalized here. The mandatrophy is resolved not by reclassifying as rope but by recognizing the snare as the stable natural classification — the six-perspective variance shows the snare is the ground truth from the extraction flow's structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_illusion_boundary,
    'At what point does regulatory consent theater (GDPR/CCPA consent notices) become functionally equivalent to coerced acceptance, rendering the consent-based extraction illegitimate?',
    'Empirical analysis of consent rate manipulation (interface design, default settings, friction), correlation between stated preferences and actual behavior, audit of whether consent withdrawal actually stops data collection',
    'If consent is illusory: snare classification confirmed even at regulatory perspective. If consent is functional: tangled_rope more justified. Changes attribution of extraction responsibility (platforms vs regulators vs users).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_illusion_boundary, empirical, 'Whether consent-based extraction is functionally coerced').

omega_variable(
    network_externality_trap,
    'Are users trapped by positive network externalities (switching cost impossibility) or merely constrained by coordination cost (could leave if willing to accept communication friction)?',
    'Historical analysis of platform switching costs over time, comparative study of users who successfully migrated to alternatives, measurement of communication loss from defection',
    'If trapped: exit_options should be ''trapped'' not ''constrained''; d increases; snare classification strengthens. If constrained: tangled_rope more appropriate for some user cohorts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_externality_trap, empirical, 'Network externality as trap vs. constraint').

omega_variable(
    identity_lock_persistence,
    'Does digital native identity lock persist through adolescence into adulthood, or is it developmentally contingent (unlocks with cognitive maturation)?',
    'Longitudinal psychological studies tracking platform dependence and identity fusion across age cohorts; analysis of users who successfully developed platform-independent identity; measurement of intrinsic vs. extracted motivation for platform use',
    'If persistent: identity_locked classification justified; early-cohort extraction is existential. If developmental: snare applies primarily to children/adolescents; classification shifts to tangled_rope or rope for cognitively mature users who choose continued use.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether identity lock from early digital socialization persists').

omega_variable(
    alternative_coordination_efficiency,
    'Can decentralized surveillance-resistant alternatives (federated platforms, peer-to-peer protocols, local-first computing) achieve equivalent coordination efficiency to centralized platforms, or is the efficiency gap inherent?',
    'Comparative performance analysis: latency, scalability, feature completeness, user experience friction; measurement of how network effects scale in decentralized vs centralized architectures; economic analysis of sustainability models',
    'If equivalence possible: scaffold sunset is robust; centralized surveillance model is contingent, not necessary. If centralized has inherent efficiency advantage: scaffold is aspirational; sunset is slower or impossible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_efficiency, empirical, 'Whether decentralized alternatives can match centralized efficiency').

omega_variable(
    behavioral_data_irreplacability,
    'Is behavioral data fundamentally irreplaceable for specific coordination functions (fraud detection, security), or are these coordination needs addressed through behavioral extraction but could use alternative mechanisms?',
    'Technical analysis: audit which platform functions require behavioral data vs. which use it for optimization; comparison with alternative systems (authentication without tracking, recommendation without profiling, fraud detection via patterns vs. individuals)',
    'If irreplaceable: extraction is coordination cost (rope classification justified). If replaceable: extraction is pure rent-seeking (snare classification justified). Changes whether behavioral data is a feature or a bug of platform architecture.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_data_irreplacability, empirical, 'Whether behavioral data is essential for claimed coordination functions').

omega_variable(
    state_surveillance_decoupling,
    'Can state surveillance access to behavioral data be structurally decoupled from commercial surveillance infrastructure, or are they necessarily coupled?',
    'Legal/technical analysis: audit actual data sharing between platforms and state; measurement of state access requests vs. platform resistance; comparison of countries with/without constitutional privacy protection; assessment of alternative state intelligence architectures',
    'If decoupled: snare classification can be addressed by separating commercial from state extraction; scaffold sunset addresses commercial extraction separately from state power. If coupled: state surveillance leverages commercial extraction; addressing snare requires addressing both simultaneously.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_surveillance_decoupling, empirical, 'Whether commercial and state surveillance can be decoupled').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behavioral_data_surveillance, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(behav_surv_tr_t0, behavioral_data_surveillance, theater_ratio, 0, 0.28).
narrative_ontology:measurement(behav_surv_tr_t5, behavioral_data_surveillance, theater_ratio, 5, 0.42).
narrative_ontology:measurement(behav_surv_tr_t10, behavioral_data_surveillance, theater_ratio, 10, 0.58).
narrative_ontology:measurement(behav_surv_tr_t15, behavioral_data_surveillance, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(behav_surv_be_t0, behavioral_data_surveillance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(behav_surv_be_t5, behavioral_data_surveillance, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(behav_surv_be_t10, behavioral_data_surveillance, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(behav_surv_be_t15, behavioral_data_surveillance, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behavioral_data_surveillance, resource_allocation).
narrative_ontology:affects_constraint(behavioral_data_surveillance, algorithmic_recommendation_capture).
narrative_ontology:affects_constraint(behavioral_data_surveillance, filter_bubble_polarization).
narrative_ontology:affects_constraint(behavioral_data_surveillance, data_broker_secondary_markets).
narrative_ontology:affects_constraint(behavioral_data_surveillance, digital_identity_fragmentation).

% DUAL FORMULATION NOTE:
% Behavioral data surveillance decomposes into multiple constraint families at different architectural levels: (1) Commercial behavioral surveillance (this story) with platform operators as beneficiaries; (2) State mass surveillance accessing commercial data infrastructure with security apparatus as beneficiary; (3) Behavioral targeting in advertising networks with advertiser extraction; (4) Digital identity formation mechanics in native youth (identity_locked cohort). Each has distinct epsilon reflecting different measurement domains and institutional actors. This story focuses on commercial extraction flow. State surveillance forms a separate story (upstream) where d-values are rederived with security apparatus as primary beneficiary.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(behavioral_data_surveillance, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
