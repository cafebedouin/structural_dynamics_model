% ============================================================================
% CONSTRAINT STORY: digital_advertising_surveillance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_advertising_surveillance, []).

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
 *   constraint_id: digital_advertising_surveillance
 *   human_readable: Digital Advertising Surveillance Infrastructure
 *   domain: technology/political_economy
 *
 * SUMMARY:
 *   Digital advertising surveillance represents a hybrid constraint combining
 *   genuine coordination (matching advertisers to interested consumers) with
 *   systematic extraction (behavioral data capture, attention rent,
 *   informational power asymmetry). The constraint exhibits the structural
 *   signature of tangled rope: platforms solve a real targeting problem while
 *   extracting surplus through behavioral tracking; suppression operates
 *   through both structural barriers (exit from digital services is costly)
 *   and internalized frames (consent theater, inevitability narratives);
 *   enforcement is active (algorithmic systems, data infrastructure) but
 *   relies on obscurity rather than legal authority. The constraint's theater
 *   ratio (0.55) reflects that consent mechanisms, privacy policies, and
 *   cookie banners create performative user agency while preserving
 *   underlying extraction. Extractiveness has increased from 0.35 to 0.58
 *   over twenty years as behavioral tracking has expanded from simple cookie
 *   data to cross-device profiles, real-time bidding, lookalike audiences,
 *   and predictive modeling. Suppression has remained high (0.68) because
 *   consumer exit requires abandoning social media, email, search,
 *   navigation, and online commerce — participation in the digital economy is
 *   increasingly non-negotiable.
 *
 * KEY AGENTS:
 *   - Individual Consumers: Primary victims (powerless/trapped) — cannot exit digital services without severe participation penalty; tracked by default with performative consent
 *   - Small Business Advertisers: Secondary actors (moderate/constrained) — benefit from targeting capability but also face extraction through platform fees; constrained exit
 *   - Advertising Platforms: Primary beneficiaries (institutional/arbitrage) — core business logic depends on behavioral data; maximum exit optionality and extraction advantage
 *   - Data Brokers: Secondary beneficiaries (institutional/arbitrage) — intermediate layer selling aggregated profiles; operate with minimal regulatory friction
 *   - Privacy Regulation Coalition: Organized opponents (organized/constrained) — building alternative pathways through GDPR, CCPA, DMA, DPA; sunset logic of privacy-protective regulation
 *   - Regulators: Captured institutional actor (institutional/constrained) — dependent on ad-tech revenue for digital services ecosystem funding; identity-locked into surveillance-dependent economy
 *   - Consumer Autonomy (Abstract Victim): Cannot organize; bears cost of behavioral profiling and manipulation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_advertising_surveillance, 0.58).
domain_priors:suppression_score(digital_advertising_surveillance, 0.68).
domain_priors:theater_ratio(digital_advertising_surveillance, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_advertising_surveillance, extractiveness, 0.58).
narrative_ontology:constraint_metric(digital_advertising_surveillance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(digital_advertising_surveillance, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_advertising_surveillance, tangled_rope).
narrative_ontology:human_readable(digital_advertising_surveillance, "Digital Advertising Surveillance Infrastructure").
narrative_ontology:topic_domain(digital_advertising_surveillance, "technology/political_economy").

domain_priors:requires_active_enforcement(digital_advertising_surveillance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_advertising_surveillance, advertising_platforms).
narrative_ontology:constraint_beneficiary(digital_advertising_surveillance, data_brokers).
narrative_ontology:constraint_beneficiary(digital_advertising_surveillance, targeted_advertisers).
narrative_ontology:constraint_victim(digital_advertising_surveillance, consumer_autonomy).
narrative_ontology:constraint_victim(digital_advertising_surveillance, informational_privacy).
narrative_ontology:constraint_victim(digital_advertising_surveillance, attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SURVEILLED CONSUMER (SNARE) — Individual users cannot exit digital services without abandoning essential economic and social participation. Tracking is ubiquitous, consent mechanisms are performative (dark patterns, pre-checked boxes, incomprehensible terms), and behavioral data extraction is the operational core of the ad-tech stack. Maximum experienced extraction with minimal coordination benefit and negligible exit options.
constraint_indexing:constraint_classification(digital_advertising_surveillance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL BUSINESS ADVERTISER (TANGLED ROPE) — Faces high cost to exit (search visibility requires platform advertising), but also depends on platform's targeting mechanisms for cost-effective customer acquisition. Extraction and coordination coexist: the platform extracts attention rent while solving the SMB's targeting problem. Constrained exit with genuine but asymmetric benefit.
constraint_indexing:constraint_classification(digital_advertising_surveillance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ADVERTISING PLATFORM (ROPE) — Experiences the surveillance infrastructure as pure coordination: matching advertisers to interested consumers solves a genuine targeting problem. The platform's extraction accrues to it, but from the platform's perspective it is solving market inefficiency. Maximum beneficiary with exit arbitrage — can redirect data collection, change targeting algorithms, or pivot business models without existential loss.
constraint_indexing:constraint_classification(digital_advertising_surveillance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY REGULATION COALITION (SCAFFOLD) — Organized actors (GDPR, CCPA, DMA, civil society) see surveillance as a temporary coordination failure being solved through regulation. Privacy-by-design mandates, consent mechanisms, data minimization requirements, and transparency rules are building alternative pathways. Suppression is high (regulatory complexity, compliance costs) but has a sunset clause: as privacy-protective technologies mature and user agency increases, the extraction mechanism loses force.
constraint_indexing:constraint_classification(digital_advertising_surveillance, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSENT THEATER APPARATUS (PITON) — Cookie banners, consent dialogs, privacy policies, and opt-out mechanisms are largely performative. They create the appearance of user agency while maintaining the underlying extraction — users accept because rejecting is friction, policies are incomprehensible by design, opt-out is buried in settings. The apparatus persists through institutional inertia despite being recognized as dysfunctional by regulators and researchers alike.
constraint_indexing:constraint_classification(digital_advertising_surveillance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CAPTURED REGULATOR (TANGLED ROPE) — Regulators depend on ad-tech revenue models for content ecosystem funding (media literacy programs, public broadcasting, digital services), while also tasked with constraining extraction. Identity-locked into the platform-dependent digital economy, constrained exit. Genuine coordination function (setting rules) coexists with extraction (regulatory capture, industry influence on rule design, enforcement gaps).
constraint_indexing:constraint_classification(digital_advertising_surveillance, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / OPTIMIZATION NATURALIZING VIEW (MOUNTAIN) — From a civilization-scale view, some matching of consumer interest to advertiser message is 'efficient' by economic definition. This perspective frames surveillance as an immutable law of information economics: asymmetric information creates screening mechanisms; targeting is the solution to information asymmetry; extraction is the natural price. The engine's false summit detector will flag this as naturalizing what is actually a contingent policy choice (privacy-protective alternatives exist).
constraint_indexing:constraint_classification(digital_advertising_surveillance, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_advertising_surveillance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(digital_advertising_surveillance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(digital_advertising_surveillance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(digital_advertising_surveillance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(digital_advertising_surveillance, TR),
    TR >= 0.70.

:- end_tests(digital_advertising_surveillance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Behavioral data extraction generates substantial surplus for platforms — behavioral-targeted ads command 3-5x premium over contextual ads. This is measurable, sustained extraction. However, extractiveness is not maximal (0.72+) because: (1) some legitimate coordination value exists (advertisers do benefit from targeting, consumers do sometimes find relevant ads), (2) alternative business models are theoretically viable (subscription, contextual advertising), and (3) regulatory pressure is increasing enforcement costs. The upward trajectory (0.35→0.58 over twenty years) reflects deepening of behavioral tracking, expansion of cross-device profiling, and sophistication of inference algorithms. Suppression (0.68): High. Structural barriers include switching costs (integrating out of digital services), information asymmetry (users don't know what data is collected), and apparent inevitability (all platforms use surveillance). Internalized suppression includes narratives ('I have nothing to hide,' 'free services are the tradeoff,' 'surveillance is inevitable'). Theater ratio (0.55): Moderate. Consent mechanisms are substantially performative — cookie banners use dark patterns (pre-checked boxes, buried opt-out), privacy policies are incomprehensible by design, and opt-out friction is intentional. However, the underlying extraction mechanism (algorithmic tracking, data aggregation) is functionally efficient (not purely theatrical), so theater doesn't reach piton levels. Tangedness (tangled rope gate): Genuine coordination function exists (advertiser-consumer matching), at least one beneficiary (platforms), at least one victim (consumer autonomy), and active enforcement (algorithmic systems with legal backing).
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence: consumer sees snare; platform sees rope; regulator sees tangled rope; privacy coalition sees scaffold; consent apparatus sees piton; observer risks mountain. This spread indicates genuine structural hybridity (tangled rope is correct) rather than misclassification. Different agents experience the same constraint as serving radically different functions: extraction (consumer view), coordination (platform view), regulation failure (regulator view), temporary problem with solution (coalition view), ritual theater (apparatus view), economic law (observer risk). The perspectival gaps reveal the real structural tensions: platforms cannot maintain cooperation with consumers without suppressing their perception of extraction; regulators cannot maintain legitimacy without hiding their capture; observers cannot maintain analytical neutrality without naturalizing extraction as optimization.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position: who bears costs, who receives benefits, and what exit options they have. Platforms and data brokers are beneficiaries with arbitrage exit (can change business models, redirect data, or exit markets) — they experience low or negative effective extraction. Consumers are victims with trapped exit (participation in digital economy is non-negotiable) — they experience high effective extraction. Small advertisers are ambiguous: they benefit from targeting (lower acquisition cost) but are also extracted from (platform fees, data leverage), with constrained exit — they experience moderate extraction. Regulators are institutional actors with constrained exit (embedded in ad-tech ecosystem, budget-dependent on platform revenue) and victim status regarding their own regulatory function (captured by industry) — complex directionality between 0.40-0.60. The computational chain: beneficiary status + arbitrage exit → low d → negative f(d); victim status + trapped exit → high d → high f(d); mixed status + constrained exit → moderate d → moderate f(d).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that tangled rope classification is correct and stable across the structural data. The temptation is to classify as pure snare (from consumer) or pure rope (from platform). The mandatrophy resolves by noting: (1) genuine coordination function exists (targeting does solve information asymmetry for all three parties: advertiser finds audience, consumer sees relevant ads, platform connects them), (2) asymmetric extraction also exists (behavioral data creation extracts surplus from consumers who don't choose it, platforms capture disproportionate gains), (3) beneficiaries and victims are distinct groups (not same actor experiencing both symmetrically), (4) enforcement is active but requires suppression (consent theater, dark patterns). The constraint is neither pure coordination nor pure extraction — it is coordination infrastructure weaponized for extraction. This is the definition of tangled rope: a mechanism that solves a genuine collective problem AND extracts asymmetric surplus, with both functions essential to the constraint's persistence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_data_necessity,
    'Is fine-grained behavioral tracking necessary for effective advertising, or is it an optimization layered onto coarser targeting?',
    'Comparative analysis of ad effectiveness across privacy-protective targeting (contextual, keyword-based, minimal behavioral data) vs. behavioral surveillance; real-world trials with differential data retention policies',
    'If necessary: suppression justified as coordination cost (raise floor). If optimization: tracking is pure extraction overhead — lower claimed extractiveness, reclassify toward snare from advertiser perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(behavioral_data_necessity, empirical, 'Whether behavioral tracking is necessary or optimized extraction').

omega_variable(
    consent_mechanism_functionality,
    'Do privacy regulation consent mechanisms (GDPR, CCPA) actually change user data exposure, or are they theater that preserves extraction while creating compliance cost?',
    'Measurement of actual data collection pre/post regulation; analysis of GDPR/CCPA opt-out rates and resulting reduction in tracking; platform behavioral response to consent requirements',
    'If functional: scaffold perspective confirmed, sunset is real. If theater: piton classification for the regulator, and extractiveness for consumers increases (suppression via false choice).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_mechanism_functionality, empirical, 'Whether consent mechanisms actually reduce data extraction').

omega_variable(
    alternative_business_model_viability,
    'Can advertising-supported digital services sustain themselves with privacy-protective data practices (subscription, contextual targeting, federated learning), or does the business model require behavioral surveillance?',
    'Longitudinal analysis of platforms with privacy-first models (Duck Duck Go, privacy-focused browsers) market viability; cost structure comparison between behavioral and contextual targeting infrastructure',
    'If viable: extraction is choice rather than necessity — reclassify toward snare for all but platform. If unviable without surveillance: extraction is coordination cost, and platform perspective (rope) is correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_business_model_viability, empirical, 'Whether privacy-protective business models are viable').

omega_variable(
    consumer_identity_lock,
    'Are consumers trapped by structural necessity (no alternatives) or identity-locked by internalized narratives (''free services are normal,'' ''I have nothing to hide,'' ''surveillance is inevitable'')?',
    'Qualitative analysis of consumer mental models; controlled exposure to privacy alternatives; measurement of defection to privacy-protective services when switching costs are reduced',
    'If trapped: snare persists. If identity-locked: consumers are structurally mobile but cognitively captured — creates potential for reframing campaigns to break lock.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consumer_identity_lock, empirical, 'Whether consumer exit barriers are structural or cognitive').

omega_variable(
    regulatory_capture_mechanism,
    'Is the regulator genuinely constrained by identity fusion with the ad-tech ecosystem, or do they have structural autonomy but choose not to enforce?',
    'Analysis of enforcement gaps, industry influence on rule design, regulator recruitment from industry, budget constraints; comparison to regulators with structural autonomy (privacy ombudsmen, DPA without ad-tech ecosystem dependence)',
    'If identity-locked: captured regulator perspective (tangled rope) is correct. If choice: reclassify to institutional beneficiary or complicit observer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether regulator capture is structural or chosen').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_advertising_surveillance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digads_tr_t0, digital_advertising_surveillance, theater_ratio, 0, 0.4).
narrative_ontology:measurement(digads_tr_t10, digital_advertising_surveillance, theater_ratio, 10, 0.48).
narrative_ontology:measurement(digads_tr_t20, digital_advertising_surveillance, theater_ratio, 20, 0.55).
narrative_ontology:measurement(digads_tr_t5, digital_advertising_surveillance, theater_ratio, 5, 0.44).
narrative_ontology:measurement(digads_tr_t15, digital_advertising_surveillance, theater_ratio, 15, 0.52).

% Extraction over time
narrative_ontology:measurement(digads_be_t0, digital_advertising_surveillance, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(digads_be_t10, digital_advertising_surveillance, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(digads_be_t20, digital_advertising_surveillance, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(digads_be_t5, digital_advertising_surveillance, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(digads_be_t15, digital_advertising_surveillance, base_extractiveness, 15, 0.53).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(digital_advertising_surveillance, resource_allocation).
narrative_ontology:boltzmann_floor_override(digital_advertising_surveillance, 0.18).
narrative_ontology:affects_constraint(digital_advertising_surveillance, attention_extraction_via_algorithmic_feeds).
narrative_ontology:affects_constraint(digital_advertising_surveillance, platform_lock_in_and_switching_costs).
narrative_ontology:affects_constraint(digital_advertising_surveillance, regulatory_capture_in_tech_sectors).

% DUAL FORMULATION NOTE:
% The digital advertising surveillance constraint family decomposes into three structurally distinct constraints: (1) advertising_surveillance_data_extraction (ε≈0.58, core tangled rope) focusing on the data pipeline, (2) attention_extraction_via_algorithmic_feeds (ε≈0.65, snare) focusing on content amplification mechanisms that serve extraction, and (3) platform_lock_in_and_switching_costs (ε≈0.52, tangled rope) focusing on participation barriers. Each has different empirical status, different omega variables, and different regulatory pathways. They are linked by network relationships: surveillance enables targeting, which justifies lock-in; lock-in increases surveillance value; suppression mechanisms operate across all three. This story focuses on the core data extraction mechanism; sister stories cover feed manipulation and switching cost extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(digital_advertising_surveillance, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
