% ============================================================================
% CONSTRAINT STORY: user_data_commercialization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_user_data_commercialization, []).

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
 *   constraint_id: user_data_commercialization
 *   human_readable: User Data Commercialization
 *   domain: digital_economy/privacy/technology
 *
 * SUMMARY:
 *   User data commercialization represents the systematic extraction of
 *   behavioral, preference, and identity data from digital platform users,
 *   monetized through advertising networks, data brokers, and downstream
 *   buyers. The constraint exhibits the full spectrum of DR classification
 *   depending on structural position: immutable law of digital markets (from
 *   civilizational view), temporary coordination problem with alternative
 *   pathways (from privacy advocates), genuine coordination mechanism (from
 *   platforms), mixed extraction-coordination hybrid (from regulators and
 *   privacy-conscious users), pure extraction (from structurally trapped
 *   users), and degraded theater (from the consent/privacy disclosure
 *   apparatus). The extractiveness has increased from 0.35 to 0.58 over the
 *   interval as data collection has deepened, new monetization channels have
 *   opened (algorithmic discrimination, credit scoring, behavioral
 *   prediction), and suppression mechanisms (dark patterns, policy
 *   obfuscation, regulatory capture) have become more sophisticated. The
 *   theater ratio has risen from 0.38 to 0.62 as platforms invest heavily in
 *   visible privacy controls, consent dialogs, and policy documentation while
 *   simultaneously employing design patterns that nullify these apparent
 *   protections. This is the diagnostic signature of institutional drift
 *   toward piton status: the machinery of user control persists and grows
 *   more elaborate, while its functional capacity to protect privacy or
 *   enable genuine choice declines.
 *
 * KEY AGENTS:
 *   - Individual Users: Primary victims (powerless/trapped) — structurally dependent on platforms with no viable alternatives; face total extraction with no exit option except social marginalization
 *   - Privacy-Conscious Users: Secondary victims (moderate/constrained) — can partially exit at high cost; experience mixed coordination-extraction due to genuine network benefits alongside asymmetric data monetization
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — experience data monetization as legitimate coordination mechanism enabling service delivery; have high exit optionality if business model changes required
 *   - Advertising Networks and Data Brokers: Secondary beneficiaries (institutional/arbitrage) — extract value from platform data monetization; face no direct user relationship and zero suppression of their operations
 *   - Regulatory Bodies: Complex actors (organized/constrained) — attempt coordination through privacy rules but face regulatory capture by large platforms; themselves experience extraction through compliance burden that favors incumbents
 *   - Privacy-Preserving Alternatives: Organized agents (organized/mobile) — offer scaffold pathway with sunset logic; remain niche due to network effect disadvantage but represent genuine alternative if critical mass achieved
 *   - Consent and Privacy Disclosure System: Institutional machinery (institutional/arbitrage) — maintains elaborate theater of user control and informed consent; persists through regulatory satisfaction rather than functional privacy protection
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing advertising-dependent monetization as inherent to digital services rather than contingent business model choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(user_data_commercialization, 0.58).
domain_priors:suppression_score(user_data_commercialization, 0.68).
domain_priors:theater_ratio(user_data_commercialization, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(user_data_commercialization, extractiveness, 0.58).
narrative_ontology:constraint_metric(user_data_commercialization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(user_data_commercialization, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(user_data_commercialization, tangled_rope).
narrative_ontology:human_readable(user_data_commercialization, "User Data Commercialization").
narrative_ontology:topic_domain(user_data_commercialization, "digital_economy/privacy/technology").

domain_priors:requires_active_enforcement(user_data_commercialization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(user_data_commercialization, platform_operators).
narrative_ontology:constraint_beneficiary(user_data_commercialization, advertising_networks).
narrative_ontology:constraint_beneficiary(user_data_commercialization, data_brokers).
narrative_ontology:constraint_victim(user_data_commercialization, individual_users).
narrative_ontology:constraint_victim(user_data_commercialization, user_privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL USER (SNARE) — Users are structurally trapped by network effects and platform dependency. Declining service terms means losing access to critical digital services (communication, commerce, identity verification). No meaningful alternative ecosystem exists. Suppression is high: data collection is opaque, consent mechanisms are performative theater (dark patterns, buried settings, pre-checked boxes), and users lack technical capacity to verify or control data flows. The extraction is experienced as total — users bear all costs (privacy loss, manipulation, discrimination) while receiving services that have become economically essential.
constraint_indexing:constraint_classification(user_data_commercialization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS COHORT (TANGLED ROPE) — Users with technical knowledge and resources can partially exit (use VPNs, privacy tools, data minimization practices). But exit carries real costs: reduced service functionality, social friction (friends on dominant platforms), employment risk (applications often require account creation). Also benefits from platform coordination: calendar sync, photo backup, messaging standards. The constraint is hybrid — genuine coordination (network effects require shared standards) alongside asymmetric extraction (data monetization is one-sided).
constraint_indexing:constraint_classification(user_data_commercialization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATORS (ROPE) — Platforms experience data monetization as a coordination mechanism: aggregated user data enables personalized service delivery, network effect amplification, and algorithmic improvement. The constraint provides genuine coordination benefits (matching users to content, spam detection, fraud prevention). Platforms have high exit optionality — they can shift business models, change data practices, or migrate to alternative monetization. The extraction is experienced as fair exchange: data in return for free or subsidized services. No suppression from the platform's perspective.
constraint_indexing:constraint_classification(user_data_commercialization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODIES (TANGLED ROPE) — Regulators (EU GDPR, state privacy laws) attempt to coordinate data practices while also facing extraction pressures. They have genuine coordination functions: setting standards, enabling interoperability, protecting vulnerable users. But they also extract through compliance burden (regulatory capture by large platforms that can afford compliance costs), data localization requirements that advantage domestic platforms, and bureaucratic overhead that benefits incumbent firms. Regulators have constrained exit — they cannot simply ban data monetization without disrupting service delivery, but they also cannot fully exit the extraction mechanism because platforms continue to lobby for favorable terms.
constraint_indexing:constraint_classification(user_data_commercialization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: PRIVACY-PRESERVING ALTERNATIVES (SCAFFOLD) — Federated platforms (Mastodon, Matrix), privacy-focused services (Signal, DuckDuckGo), and decentralized protocols represent a temporary coordination mechanism with a sunset clause. These platforms offer lower-extraction alternatives by design (minimal data collection, user-owned data, decentralized governance). Their extraction is low and declining as they mature. However, they face the critical scaffold challenge: they cannot compete on network effects alone and require active user migration to achieve critical mass. The sunset condition is that network effects eventually transition users to the alternative ecosystem or the incumbent platforms adopt sufficient privacy protections that the need for alternatives fades.
constraint_indexing:constraint_classification(user_data_commercialization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSENT/PRIVACY DISCLOSURE THEATER (PITON) — Privacy policies, consent dialogs, and user settings are substantially performative. Users cannot meaningfully read or understand privacy policies (average length 76+ pages, comprehension time 200+ hours per year). Consent mechanisms use dark patterns and cognitive friction to achieve pre-checked defaults. Privacy settings reset with updates. The machinery persists through legal compliance theater: platforms demonstrate 'user control' and 'informed consent' to satisfy regulatory requirements, not to enable actual user agency. Theater ratio is high (0.62) because the visible apparatus (settings, policies, consent buttons) occupies significant platform engineering effort while providing minimal functional privacy protection. The system is maintained because it satisfies regulatory appearance requirements, not because it works.
constraint_indexing:constraint_classification(user_data_commercialization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, data monetization appears as an immutable law of digital markets: the economic model of free services requires revenue, data is the natural residual value, and users cannot simultaneously access free services and prevent data extraction. This perspective risks naturalizing what is actually a contingent institutional design choice — the dominance of advertising-based monetization over subscription, donation, or public-utility models. The false summit detector will identify this as misplaced naturalization.
constraint_indexing:constraint_classification(user_data_commercialization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(user_data_commercialization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(user_data_commercialization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(user_data_commercialization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(user_data_commercialization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(user_data_commercialization, TR),
    TR >= 0.70.

:- end_tests(user_data_commercialization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Users lose privacy, autonomy, and behavioral control; platforms gain revenue from user-generated behavioral data. The extraction is not total (0.72+) because platforms do provide valuable services and some users benefit from personalization. But the extraction is substantial because users do not have genuine choice and data monetization is not proportional to service value. The rising trajectory (0.35 → 0.58) reflects that platforms have expanded data collection beyond advertising into credit scoring, behavioral prediction, employment screening, and government contracts, multiplying harms beyond the original quid pro quo of personalized advertising. Suppression (0.68): High. Users face multiple suppression mechanisms: (1) Opacity — platform data practices are not disclosed in comprehensible form. Average privacy policy is 76+ pages and requires 200+ hours/year of reading to understand all platforms. (2) Dark patterns — consent dialogs use cognitive friction, pre-checked boxes, and designed confusion to achieve platform-favorable defaults. (3) Technical incapacity — most users lack ability to understand or verify data collection at technical level. (4) Institutional dependence — platforms have become essential for employment, education, commerce, social connection, government services. (5) Network lock-in — switching costs are prohibitively high due to social network effects, data portability barriers, and interoperability absence. (6) Regulatory capture — privacy laws create compliance burden that favors large incumbents, making exit toward alternatives even more difficult. Theater ratio (0.62): Moderate-high and rising. Privacy policies, consent dialogs, privacy settings, data deletion buttons, and regulatory compliance machinery are elaborate. But their functional capacity to protect privacy or enable meaningful user choice is minimal. Privacy policies are unreadable and legally privileged to favor platforms. Consent dialogs use dark patterns. Privacy settings often reset with updates or are bypassed through backend data collection. The growing theater (0.38 → 0.62) reflects that as regulatory pressure has increased, platforms invest more resources in visible privacy machinery while simultaneously employing more sophisticated means to nullify its effect. This is classic piton drift: elaborate machinery that appears functional but has atrophied in actual protective capacity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence because structural position completely determines classification. The trapped user and the platform operator are locked into incompatible perspectives: one sees a Snare, the other sees a Rope, and both are correct for their position. The regulatory/alternative perspectives split the difference. The analytical observer risks false summit by naturalizing advertising-dependent monetization as inherent to digital economics rather than as a contingent market design choice. The gap persists because exit options differ genuinely — users cannot exit without social/economic cost; platforms can exit without cost if business model changes; alternatives can scale if network effects can be overcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) measures each agent's structural position relative to extraction flow. Individual users (powerless/trapped): beneficiary/victim status is pure victim, trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → maximum experienced extraction. Privacy-conscious users (moderate/constrained): mixed beneficiary/victim status (some service benefit, significant data cost), constrained exit → d ≈ 0.65 → f(d) ≈ 1.00 → moderate extraction. Platform operators (institutional/arbitrage): beneficiary status, arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative extraction (they experience the constraint as beneficial). Regulators (organized/constrained): mixed beneficiary/victim (coordinating privacy rules but captured), constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 → moderate extraction. Privacy alternatives (organized/mobile): temporary role, mobile exit → d ≈ 0.35 → f(d) ≈ 0.30 → low extraction. Consent apparatus (institutional/arbitrage): beneficiary of regulatory requirement, arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.08 → negative extraction (it persists because regulation requires it). Spatial scope: global (σ = 1.2) because platforms operate globally, user data is traded globally, and alternative platforms must achieve global network effects. The global scope amplifies effective extraction for trapped users but moderates it for alternatives (who benefit from decentralized, non-territorial design).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the classification depends entirely on structural position (exit options, power, beneficiary/victim status). The mandatrophy would arise if the analyst tried to declare a single 'true' classification: Is it coordination (rope) or extraction (snare)? It is both, simultaneously, from different positions. The resolution is to measure from each position separately. For trapped users, it is unambiguously a snare (ε ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66). For platforms, it is unambiguously coordination (ε in coordination range for rope, suppression near zero). For moderates and regulators, it is genuinely tangled — both functions are present. The analytics are not contradictory; they are position-dependent. The false summit (mountain from civilizational view) is a separate diagnostic: the constraint appears immutable and natural ('free services require data extraction') only when measured from a context that has no structural position in the extraction flow. When measured from within (user, platform, regulator), its contingency is visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_meaningfulness_threshold,
    'What constitutes meaningful consent vs performative consent in digital data collection contexts?',
    'Empirical study: user comprehension rates of actual privacy policies; correlation between stated preferences (privacy settings) and actual data collection; cross-platform consistency of user understanding of data flows',
    'If consent is demonstrably non-meaningful: snare classification strengthens, theater ratio rises. If meaningful consent mechanisms can be designed: tangled_rope transitions toward rope for participating users.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_meaningfulness_threshold, empirical, 'Threshold for meaningful vs performative digital consent').

omega_variable(
    alternative_platform_critical_mass,
    'What network size is required for privacy-preserving alternative platforms to become viable primary services rather than niche tools?',
    'Network effects analysis; historical data on platform migration tipping points; measurement of switching costs vs benefit differentials; demographic analysis of early adopters vs mainstream user requirements',
    'If critical mass is achievable (10-50M users): scaffold sunset is realistic and timeline estimates are correct. If critical mass is unachievable (<1M users sustainable): scaffold perspective is aspirational theater, platforms remain locked-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_critical_mass, empirical, 'Network size threshold for alternative platform viability').

omega_variable(
    data_monetization_necessity,
    'Is advertising-based data monetization economically necessary for service delivery, or is it a choice that maximizes profit relative to subscription or public-utility models?',
    'Business model analysis: comparative economics of subscription services vs ad-supported services; user willingness-to-pay studies; profitability analysis of alternative models (Wikipedia, Signal, Mozilla); regulatory impact of forcing service providers toward alternative revenue models',
    'If necessary: data monetization is partly justified as efficient resource allocation (moderate extraction). If optional: data monetization is pure rent-seeking extractive choice (high extraction, snare strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(data_monetization_necessity, conceptual, 'Economic necessity vs optionality of advertising-based monetization').

omega_variable(
    regulatory_capture_depth,
    'To what degree do privacy regulations themselves become captured by large platforms, functioning as barriers to entry that entrench incumbent market power?',
    'Historical analysis of regulatory burden: compliance cost by firm size; correlation between regulatory changes and platform market share; evidence of regulatory text authored by or favorable to incumbent platforms; emergence of privacy compliance as a cost-advantage tool',
    'If capture is high: regulatory perspective (organized/constrained) becomes victim rather than partial beneficiary, tangled_rope transitions toward snare. If capture is low: regulatory function is genuine coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_depth, empirical, 'Degree of regulatory capture in privacy regimes').

omega_variable(
    user_agency_threshold,
    'At what point of technical complexity and social friction does exit from dominant platforms become genuinely mobile (user choice) vs constrained (social/economic coercion)?',
    'User studies: friction measurement (time, cost, social impact of switching); demographic analysis of who can exit vs who cannot; employer/institution policies requiring dominant platform participation; educational/employment opportunity loss from non-participation',
    'If mobility is achievable for most users: exit classification is mobile, extraction is moderated. If mobility is rare: trapped or identity_locked classifications become dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_agency_threshold, empirical, 'Threshold between constrained and mobile exit for platform users').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(user_data_commercialization, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udc_tr_t0, user_data_commercialization, theater_ratio, 0, 0.38).
narrative_ontology:measurement(udc_tr_t5, user_data_commercialization, theater_ratio, 5, 0.5).
narrative_ontology:measurement(udc_tr_t10, user_data_commercialization, theater_ratio, 10, 0.62).
narrative_ontology:measurement(udc_tr_t15, user_data_commercialization, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(udc_be_t0, user_data_commercialization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(udc_be_t5, user_data_commercialization, base_extractiveness, 5, 0.47).
narrative_ontology:measurement(udc_be_t10, user_data_commercialization, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(udc_be_t15, user_data_commercialization, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(user_data_commercialization, resource_allocation).
narrative_ontology:boltzmann_floor_override(user_data_commercialization, 0.18).
narrative_ontology:affects_constraint(user_data_commercialization, algorithmic_manipulation).
narrative_ontology:affects_constraint(user_data_commercialization, credit_scoring_opacity).
narrative_ontology:affects_constraint(user_data_commercialization, employment_surveillance).
narrative_ontology:affects_constraint(user_data_commercialization, behavioral_prediction_discrimination).

% DUAL FORMULATION NOTE:
% User data commercialization is upstream of multiple extractive mechanisms that depend on access to user-generated behavioral data. Algorithmic manipulation, credit scoring, employment surveillance, and behavioral prediction all depend on monetized user data flows. The base constraint (user data extraction) enables the downstream constraints. Regulators addressing any downstream constraint must address upstream data monetization, creating regulatory coupling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(user_data_commercialization, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
