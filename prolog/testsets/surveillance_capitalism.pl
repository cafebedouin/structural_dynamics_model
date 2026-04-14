% ============================================================================
% CONSTRAINT STORY: surveillance_capitalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_surveillance_capitalism, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: surveillance_capitalism
 *   human_readable: Surveillance Capitalism: Data Extraction and Behavioral Control
 *   domain: political_economy/technology/digital_rights
 *
 * SUMMARY:
 *   Surveillance capitalism represents the structural coupling of data
 *   extraction with behavioral control across digital platforms. The
 *   constraint operates through a unified mechanism: platform corporations
 *   collect granular behavioral data from users who cannot meaningfully exit,
 *   aggregate this data into psychographic profiles, and use these profiles
 *   to manipulate user behavior through targeted content, advertising, and
 *   social influence. The extraction is enforced through network effects
 *   (users cannot leave because everyone else is there), switching costs
 *   (data, relationships, and institutional embedding), and the colonization
 *   of essential digital infrastructure (communication, commerce, navigation,
 *   identity). The theater ratio has increased substantially over the
 *   interval (2010-2025) as regulators have demanded 'transparency' and
 *   platforms have responded with increasingly elaborate consent theater —
 *   privacy policies, data access requests, and 'privacy settings' that are
 *   incomprehensible or deliberately deceptive. The constraint exhibits the
 *   full range of DR classifications depending on structural position: pure
 *   extraction (Snare) from the powerless data subject's perspective,
 *   coordination (Rope) from the platform's perspective, mixed
 *   coordination-extraction (Tangled Rope) from regulatory and advertising
 *   industry perspectives, a temporary problem with sunset (Scaffold) from
 *   the privacy advocate perspective, degraded ritual (Piton) for the consent
 *   framework, and pure extraction (Snare again) from the civilizational
 *   analytical view that recognizes the coordination benefits as secondary
 *   rationalizations.
 *
 * KEY AGENTS:
 *   - Data Subjects: Primary victims (powerless/trapped) — users of platforms (Facebook, Google, TikTok, Amazon, etc.) experiencing total suppression and maximum extraction with no viable exit
 *   - Platform Corporations: Primary beneficiaries (institutional/arbitrage) — tech giants (Meta, Google/Alphabet, Amazon, TikTok/ByteDance, Apple) capturing data, attention, and behavioral control with high exit capacity
 *   - Advertising Industry: Secondary beneficiary (powerful/mobile) — ad networks, agencies, and advertisers benefiting from behavioral targeting; constrained by regulation and privacy tech but with mobile exit options
 *   - Regulatory Agencies: Mixed actor (institutional/constrained) — GDPR enforcers, FTC, national data protection authorities attempting to coordinate public interests while partially captured by platform influence and regulatory arbitrage
 *   - Privacy-Conscious Coalition: Organized alternative builder (organized/constrained) — privacy advocates, privacy-tech developers (Signal, DuckDuckGo, Proton), digital rights organizations, decentralization protocols building Scaffold exits
 *   - Informational Autonomy (Abstract Victim): The public good of privacy and cognitive liberty that cannot organize or advocate; bears cost of information asymmetry and behavioral manipulation
 *   - Democratic Deliberation: Systemic victim (unmeasurable exit) — democratic discourse degraded by filter bubbles, algorithmic amplification, and coordinated manipulation through surveillance data
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(surveillance_capitalism, 0.68).
domain_priors:suppression_score(surveillance_capitalism, 0.72).
domain_priors:theater_ratio(surveillance_capitalism, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(surveillance_capitalism, extractiveness, 0.68).
narrative_ontology:constraint_metric(surveillance_capitalism, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(surveillance_capitalism, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(surveillance_capitalism, snare).
narrative_ontology:human_readable(surveillance_capitalism, "Surveillance Capitalism: Data Extraction and Behavioral Control").
narrative_ontology:topic_domain(surveillance_capitalism, "political_economy/technology/digital_rights").

domain_priors:requires_active_enforcement(surveillance_capitalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(surveillance_capitalism, platform_corporations).
narrative_ontology:constraint_beneficiary(surveillance_capitalism, advertising_industrial_complex).
narrative_ontology:constraint_victim(surveillance_capitalism, data_subjects).
narrative_ontology:constraint_victim(surveillance_capitalism, informational_autonomy).
narrative_ontology:constraint_victim(surveillance_capitalism, democratic_deliberation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECT (SNARE) — Users are structurally trapped. Exit from digital platforms incurs severe costs: loss of employment visibility (LinkedIn), social isolation (Facebook/Instagram), commercial access (Amazon), geographic navigation (Google Maps), communication infrastructure (WhatsApp), and institutional requirements (government services, banking, education). Suppression is total — users cannot opt out of data collection while maintaining functional participation in contemporary life. The constraint extracts behavioral data and attention continuously with minimal coordination benefit. Users experience maximum extraction with no exit pathway.
constraint_indexing:constraint_classification(surveillance_capitalism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM CORPORATION (ROPE) — Tech companies experience the surveillance architecture as a coordination mechanism. Data collection enables service optimization, personalized content delivery, and matching of advertisers to target audiences. The architecture solves the genuine problem of allocating ad inventory and optimizing user engagement. Platform operators have high exit capacity — they can adjust data collection, encryption policies, or business models. They experience the constraint as pure coordination with net benefit flow toward them.
constraint_indexing:constraint_classification(surveillance_capitalism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: REGULATORY AGENCY (TANGLED ROPE) — Government data protection authorities (GDPR enforcers, FTC, national DPAs) face mixed coordination and extraction. They coordinate genuine public interests: protecting informational autonomy, preventing manipulation, and maintaining democratic deliberation. But they are also partially captured — platforms exert regulatory arbitrage (moving infrastructure to light-touch jurisdictions), lobbying pressure, and asymmetric resources. Regulators are constrained by transnational capital mobility and political influence. They extract enforcement revenue and bureaucratic power from the constraint while also serving the public function of governance.
constraint_indexing:constraint_classification(surveillance_capitalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVERTISING INDUSTRY (TANGLED ROPE) — Ad networks and agencies benefit from behavioral targeting data while facing constraints: regulatory pressure, ad blockers, privacy-focused browsers, and brand safety concerns. They have mobile exit options (programmatic switching, first-party data strategies, contextual targeting alternatives). The surveillance architecture coordinates the matching of ads to audiences AND extracts margin through information asymmetry — advertisers don't know what data they're actually buying. Mixed benefit and cost flow.
constraint_indexing:constraint_classification(surveillance_capitalism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVACY-CONSCIOUS COALITION (SCAFFOLD) — Organized privacy advocates, privacy-focused technology developers (Signal, DuckDuckGo, Proton, Tor), and digital rights organizations are building alternative infrastructure with sunset logic. As privacy-preserving technologies (local-first computing, federated social networks, zero-knowledge proofs) mature, they provide functional exits from surveillance platforms. The coalition sees the constraint as temporary — a 10-20 year transition to privacy-respecting defaults. Extraction is tolerated only because the sunset mechanism (alternative pathways) is credible.
constraint_indexing:constraint_classification(surveillance_capitalism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSENSUAL DATA USE THEATER (PITON) — The 'informed consent' framework for data collection has degraded into performative ritual. Terms of service are deliberately incomprehensible; privacy settings are intentionally buried; consent is extracted through choice architecture (opt-out hidden, opt-in prominent) rather than genuine agreement. The legal fiction of 'consent' persists through institutional inertia despite universal recognition that users cannot meaningfully consent to surveillance. The theater is maintained because no complete alternative has replaced it, not because consent actually works.
constraint_indexing:constraint_classification(surveillance_capitalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, surveillance capitalism is a pure extraction mechanism with minimal coordination function. The data extraction, behavioral profiling, and manipulative targeting serve platform profit maximization, not social coordination needs. The public goods (efficient ad markets, service personalization) are secondary to the primary extraction goal. This perspective cuts through the 'coordination' framing and reveals the structural asymmetry: those being surveilled have no meaningful exit, those doing the surveilling have high exit capacity, suppression is total through network effects and switching costs.
constraint_indexing:constraint_classification(surveillance_capitalism, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(surveillance_capitalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(surveillance_capitalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(surveillance_capitalism, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(surveillance_capitalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(surveillance_capitalism, TR),
    TR >= 0.70.

:- end_tests(surveillance_capitalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Platforms extract granular behavioral data continuously from users who cannot refuse participation. The extraction is monetized through advertising markets where user attention and behavior are sold without explicit compensation to users. The metric reflects that the primary function of the architecture is extraction (maximizing engagement and monetizable data), with coordination benefits (service personalization, ad matching) being secondary. The value increased from 0.35 (2010, early platform era with limited data collection) to 0.68 (2025, full-scale real-time behavioral surveillance) as mobile sensors, tracking pixels, and algorithmic profiling enabled more granular extraction. Suppression (0.72): Very high. Users cannot opt out of surveillance while maintaining functional participation in contemporary life. Network effects create lock-in (everyone else is on the platform). Switching costs are prohibitive (loss of social connections, economic opportunity, institutional access). Technical barriers include ubiquitous tracking (cross-site cookies, fingerprinting, location tracking). Social barriers include professional and social norms that treat platform participation as mandatory. Regulatory capture prevents governance solutions. Alternative platforms remain marginalized. The high suppression reflects that users have no realistic exit pathway. Theater ratio (0.58): Moderate-high and increasing. The 'informed consent' framework is theatrical — privacy policies are designed to be incomprehensible, privacy settings are buried, and consent is extracted through dark patterns rather than genuine choice. Regulatory responses (GDPR's right to data access, CCPA's opt-out mechanisms) have added more theater: elaborate data download procedures and privacy preference centers that create the appearance of control without actual user agency. The ratio increased from 0.28 (2010, minimal privacy theater) to 0.58 (2025, elaborate regulatory theater) as governance has demanded transparency that platforms have met with performative compliance.
 *
 * PERSPECTIVAL GAP:
 *   The gap between the data subject's Snare and the platform's Rope is fundamental. Users see themselves as trapped with no exit; platforms see themselves as solving coordination problems. The gap reflects that the constraint's primary beneficiary (the platform) has successfully naturalized extraction as coordination. Users experience coercion; platforms experience their own architecture as voluntary participation. This is the characteristic gap of a high-suppression, high-beneficiary-exit system. The scaffold perspective introduces a temporal gap: the coalition sees the constraint as temporary (sunset 10-20 years) while the snare perspective sees it as permanent. The piton perspective reveals the institutional inertia: consent theater persists not because it works but because no complete alternative has replaced it. The analytical view (snare at civilizational scale) cuts through the coordination framing and shows that this is extraction disguised as service.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain is determined by beneficiary/victim declarations and exit options. Data subjects are declared victims with trapped exit → d ≈ 0.95 → high f(d) ≈ 1.42 (experienced extraction is maximum). Platform corporations are declared beneficiaries with arbitrage exit → d ≈ 0.05 → low f(d) ≈ -0.12 (experienced extraction is negative, i.e., they are subsidized by the constraint). Regulatory agencies are institutional beneficiaries of enforcement power but constrained (not arbitrage) → d ≈ 0.55 → moderate f(d) ≈ 0.75. The analytical observer with civilizational scope derives d ≈ 0.85 (victim-adjacent, cannot exit the system) → f(d) ≈ 1.28. The directionality overrides are not needed — the structural data produces consistent d values across all perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (Extractiveness > 0.70): The constraint resolves the mandatrophy by demonstrating that it is genuinely a Snare, not a false Snare mis-labeled as coordination. The resolution pathway: (1) Coordination hypothesis test: Does the surveillance architecture solve a genuine coordination problem (matching advertisers to audiences, personalizing services) in a way that REQUIRES extraction? Answer: Partially. The coordination benefits are real but not dependent on the current extractive architecture — privacy-preserving alternatives (local ML, federated learning) can achieve comparable service quality. (2) Exit test: Can trapped agents exit? Answer: Practically no. Network effects, switching costs, and institutional embedding make exit non-viable for most users. (3) Suppression test: Is suppression structural or can it be overcome with agency? Answer: Structural and deepening. Alternative platforms exist but remain marginalized due to network effects and platform lock-in. (4) Beneficiary asymmetry test: Is extraction asymmetric? Answer: Yes, maximal. Platform corporations capture all value; users receive services (which could be provided under privacy-preserving architectures) in exchange for behavioral data (which they cannot refuse). The mandatrophy confirms: this is a Snare, not a Tangled Rope. The coordination framing is a cover story. However, the Scaffold perspective is also valid — privacy-tech alternatives are being built and could provide a sunset mechanism. The system is Snare-to-Scaffold: currently Snare, but with credible sunset mechanisms forming. Mandatrophy is resolved because the single-perspective classification (Snare) is confirmed as accurate while the multi-perspective view (including Scaffold) reveals the temporal arc: Snare today, potential Scaffold transition in 10-20 years as privacy-first alternatives mature and regulatory arbitrage closes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_decomposition,
    'Are switching costs for data subjects primarily technical, economic, social, or identity-based?',
    'Longitudinal studies of users who attempt platform exit; measurement of time, financial cost, and relationship loss across demographic groups; identity fusion analysis for platform-native cohorts',
    'If primarily technical: privacy-tech alternatives can solve the constraint (Scaffold outlook). If primarily social/identity: the constraint operates through network effects and identity lock-in (Snare deepens). If primarily economic: regulatory intervention on advertising markets could shift the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_decomposition, empirical, 'Decomposition of switching costs by mechanism type').

omega_variable(
    coordination_necessity_claim,
    'Are the genuine coordination benefits of surveillance (service personalization, ad matching) functionally dependent on the current extractive architecture, or could they be achieved with privacy-preserving alternatives?',
    'Comparison of user experience metrics under privacy-first systems (local ML, federated learning, differential privacy) vs platform surveillance systems; measurement of service quality degradation',
    'If functionally independent: the coordination framing is cover story for extraction (Snare classification justified). If truly dependent: the constraint has genuine Tangled Rope characteristics. If partially dependent: extraction is real but coordination benefits are also real (genuinely Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_necessity_claim, empirical, 'Whether coordination benefits require surveillance architecture').

omega_variable(
    behavioral_manipulation_efficacy,
    'To what degree does behavioral profiling via surveillance actually achieve higher advertising effectiveness, and to what degree is the revenue premium extracted through information asymmetry and psychological targeting opacity?',
    'A/B testing of ad effectiveness under surveillance vs privacy-preserving systems; analysis of advertiser willingness-to-pay for opacity vs for genuine effectiveness improvement; neuroscientific measurement of manipulation vulnerability',
    'If primarily effectiveness: the extraction is coordination-adjacent (advertisers genuinely benefit). If primarily opacity rent: the extraction is pure information advantage (Snare characteristics strengthen). If mixed: the balance determines the Tangled Rope vs Snare boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_manipulation_efficacy, empirical, 'Efficacy of behavioral profiling vs information asymmetry extraction').

omega_variable(
    regulatory_arbitrage_closure,
    'Can transnational regulatory coordination close the regulatory arbitrage gap that allows surveillance platforms to operate at optimal extraction (light-touch jurisdictions, concurrent enforcement failures), or is regulatory capture structural to the constraint?',
    'Analysis of regulatory enforcement patterns under GDPR, DMA, state privacy laws; measurement of platform behavioral response to enforcement (genuine compliance vs regulatory theater); identification of jurisdictions where enforcement produces actual data practice change',
    'If arbitrage can be closed: Regulatory agencies move from Tangled Rope (captured) to Rope (effective coordination). If capture is structural: regulatory agencies remain trapped in Tangled Rope. This determines whether the constraint can be regulated or only replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_arbitrage_closure, empirical, 'Whether regulatory coordination can close arbitrage gap').

omega_variable(
    identity_lock_in_digital_natives,
    'For users socialized into surveillance platforms (Gen Z and younger), is the constraint experienced as Snare (trapped by barriers) or as identity_locked (their identity constituted through platform participation)?',
    'Qualitative interviews with digital natives about platform exit; measurement of identity fusion metrics (how much self-concept depends on platform status/metrics); longitudinal tracking of users who exit and whether they reconstruct identity outside platforms',
    'If primarily trapped: privacy-tech alternatives that lower barriers could enable exit. If primarily identity_locked: individuals need identity reconstruction, not just technical alternatives — Snare runs deeper than switching costs. This determines whether the constraint can be solved by Scaffold alternatives or requires generational turnover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_digital_natives, empirical, 'Identity fusion vs external trapping in digital natives').

omega_variable(
    consent_theater_degradation_rate,
    'How rapidly is the ''informed consent'' fiction degrading? At what threshold does institutional maintenance cost exceed institutional benefit?',
    'Tracking of regulatory-required consent compliance metrics; measurement of user cynicism and consent-fatigue; analysis of liability exposure from consent-based defense in litigation; institutional replacement cost analysis for consent alternatives',
    'If degradation is accelerating: Piton perspective is accurate and the theater may collapse, forcing constraint reframing. If degradation is stable: Piton classification holds. This determines whether reform can proceed through consent framework (theater repair) or requires architectural replacement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_theater_degradation_rate, empirical, 'Rate of degradation of consent theater maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(surveillance_capitalism, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(surv_tr_t0, surveillance_capitalism, theater_ratio, 0, 0.28).
narrative_ontology:measurement(surv_tr_t5, surveillance_capitalism, theater_ratio, 5, 0.44).
narrative_ontology:measurement(surv_tr_t10, surveillance_capitalism, theater_ratio, 10, 0.58).
narrative_ontology:measurement(surv_tr_t15, surveillance_capitalism, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(surv_be_t0, surveillance_capitalism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(surv_be_t5, surveillance_capitalism, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(surv_be_t10, surveillance_capitalism, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(surv_be_t15, surveillance_capitalism, base_extractiveness, 15, 0.74).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(surveillance_capitalism, resource_allocation).
narrative_ontology:affects_constraint(surveillance_capitalism, advertising_market_concentration).
narrative_ontology:affects_constraint(surveillance_capitalism, algorithmic_amplification_polarization).
narrative_ontology:affects_constraint(surveillance_capitalism, platform_network_effects).
narrative_ontology:affects_constraint(surveillance_capitalism, digital_identity_lock_in).

% DUAL FORMULATION NOTE:
% Surveillance capitalism decomposes into multiple structurally distinct constraints: (1) data_extraction_architecture (ε ≈ 0.68, this story) — the mechanism of behavioral data collection and monetization; (2) advertising_behavioral_manipulation (ε ≈ 0.72, downstream) — the use of profiling for targeting and influence; (3) platform_network_effects (ε ≈ 0.55, structural) — the lock-in mechanism that prevents exit; (4) consent_theater_governance (ε ≈ 0.35, institutional) — the degraded regulatory framework. Each constraint has different temporal dynamics and different exit mechanisms. This story focuses on the primary extraction architecture; downstream stories address the behavioral control and lock-in mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
