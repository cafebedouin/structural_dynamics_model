% ============================================================================
% CONSTRAINT STORY: newsletter_subscriber_privacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_newsletter_subscriber_privacy, []).

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
 *   constraint_id: newsletter_subscriber_privacy
 *   human_readable: Newsletter Subscriber Privacy Constraint
 *   domain: digital_communications/privacy
 *
 * SUMMARY:
 *   Newsletter subscriber privacy is a structural constraint where content
 *   delivery coordination is systematically coupled with behavioral data
 *   extraction. Subscribers access curated content (genuine coordination
 *   function) but are required to surrender behavioral and preference data as
 *   mandatory terms. This data flows through complex third-party networks
 *   (advertising platforms, data brokers, analytics providers) creating
 *   asymmetric value capture. The constraint exhibits active enforcement
 *   through legal consent mechanisms that are largely performative: cookie
 *   banners obscure actual data flows, privacy policies are deliberately
 *   unreadable, and opt-out mechanisms are hidden or nonfunctional.
 *   Extraction has increased over the measured interval (2015-2025) as
 *   newsletter platforms have shifted from content-focused to
 *   data-monetization models. Simultaneously, regulatory pressure (GDPR, US
 *   state privacy laws, browser cookie deprecation) is creating a sunset
 *   logic: privacy-preserving alternatives are emerging but require
 *   substantial industry reorganization. From different structural positions,
 *   the same constraint appears as pure extraction (subscriber view),
 *   coordination with extraction (publisher view), operational infrastructure
 *   (data broker view), or a temporary problem being solved (regulator view).
 *
 * KEY AGENTS:
 *   - Newsletter Subscribers: Primary victims (powerless/trapped) — structurally dependent on newsletters for content access; face opaque data extraction with minimal exit options
 *   - Privacy-Conscious Subscribers: Secondary victim (moderate/constrained) — can exit but at cost of lost content; experience mixed value (personalization benefit) with extraction costs
 *   - Newsletter Publishers: Primary beneficiary (institutional/arbitrage) — extract revenue from subscriber data; coordinate content delivery; have platform-switching optionality
 *   - Advertising Networks: Secondary beneficiary (powerful/arbitrage) — capture behavioral profiles, monetize attention, enforce tracking infrastructure; have high exit optionality
 *   - Privacy Regulation Movements: Organized actors (organized/constrained) — emerge as sunset mechanism; constrained by industry adaptation and technical circumvention
 *   - Consent Infrastructure Providers: Institutional actors (institutional/arbitrage) — maintain performative theater; persist through legal compliance requirements despite functional atrophy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(newsletter_subscriber_privacy, 0.58).
domain_priors:suppression_score(newsletter_subscriber_privacy, 0.62).
domain_priors:theater_ratio(newsletter_subscriber_privacy, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(newsletter_subscriber_privacy, extractiveness, 0.58).
narrative_ontology:constraint_metric(newsletter_subscriber_privacy, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(newsletter_subscriber_privacy, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(newsletter_subscriber_privacy, tangled_rope).
narrative_ontology:human_readable(newsletter_subscriber_privacy, "Newsletter Subscriber Privacy Constraint").
narrative_ontology:topic_domain(newsletter_subscriber_privacy, "digital_communications/privacy").

domain_priors:requires_active_enforcement(newsletter_subscriber_privacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(newsletter_subscriber_privacy, newsletter_publishers).
narrative_ontology:constraint_beneficiary(newsletter_subscriber_privacy, advertising_networks).
narrative_ontology:constraint_beneficiary(newsletter_subscriber_privacy, data_brokers).
narrative_ontology:constraint_victim(newsletter_subscriber_privacy, newsletter_subscribers).
narrative_ontology:constraint_victim(newsletter_subscriber_privacy, privacy_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBSCRIBER (SNARE) — Subscriber is structurally trapped: wants content access, faces behavioral tracking and data extraction as non-negotiable terms. Alternative newsletters require same consent patterns. High suppression: tracking is opaque, opt-out mechanisms are deliberately obscured, and data sharing chains are hidden. Subscriber bears maximum extraction cost with minimal perceived benefit or escape route.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-AWARE SUBSCRIBER (TANGLED ROPE) — Subscriber has constrained exit: can switch providers at cost of lost content continuity, curated recommendations, and community. But genuine coordination function exists—personalization algorithms do improve content relevance. Mixed experience: some extraction (data sharing without informed consent) coupled with real coordination benefit (better content matching). High enough suppression to prevent easy exit; not maximal extraction because some value genuinely flows back.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PUBLISHER (ROPE) — Experiences the constraint as pure coordination: subscription data enables operational functions (delivery, list management, engagement metrics). Publisher can arbitrage between consent models (EU GDPR compliance vs US minimal consent). Benefits from subscriber list as coordination mechanism. Net beneficiary with high exit optionality. Sees tracking infrastructure as operational necessity rather than extraction.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISING NETWORK (TANGLED ROPE) — Primary extractor: captures behavioral data, builds subscriber profiles, monetizes attention patterns. Derives income from subscriber tracking. But coordination function exists: ad networks do provide publishers with revenue source enabling free content. Active enforcement required: legal consent mechanisms, data licensing agreements, cross-platform cookie infrastructure. Asymmetric: publishers benefit from revenue, subscribers bear data extraction cost. Ad network has arbitrage optionality (can exit to other data sources) and high power.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY REGULATION (SCAFFOLD) — Organized actors (GDPR, emerging US privacy laws, browser vendors implementing tracking restrictions) see the constraint as solvable through regulatory sunset: privacy-preserving alternatives (first-party data, contextual advertising, federated learning) are emerging. High suppression currently enforced; but regulations create time-limited extraction window. Sunset clause is real: cookie deprecation (third-party cookie phase-out by 2025), privacy labels, consent requirements incrementally reduce extraction mechanisms. Organized agents have constrained exit but can build alternatives.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: CONSENT THEATER (PITON) — Cookie banners, privacy policies, and consent dialogs are largely performative. Theater ratio (0.55) reflects that consent mechanisms create legal cover rather than actual informed choice: banners obscure true data flows, policies are unreadable, consent is manufactured through dark patterns. System persists through institutional inertia: legal compliance theater maintains the extraction architecture despite knowing subscribers don't actually understand or control their data. Functional verification has collapsed—the ritual persists but its stated purpose (informed consent) has atrophied.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope: newsletter subscription mechanism coordinates content delivery (genuine function) and enables subscriber profiling extraction (asymmetric value capture). Effective extraction chi reflects both the coordination function (justifying some data use) and the extraction margin (data capture exceeding what content delivery requires). Suppression persists through opacity and manufactured consent. Classification resists the mountain fallacy (privacy extraction is not inherent to newsletters) and captures the genuine hybrid structure: coordination + extraction.
constraint_indexing:constraint_classification(newsletter_subscriber_privacy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(newsletter_subscriber_privacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(newsletter_subscriber_privacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(newsletter_subscriber_privacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(newsletter_subscriber_privacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(newsletter_subscriber_privacy, TR),
    TR >= 0.70.

:- end_tests(newsletter_subscriber_privacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary value extracted is behavioral and preference data worth significant revenue to ad networks and publishers. But the extraction is not maximal (≥0.70) because some subscribers receive genuine personalization value—recommendations improve content matching. The rising trajectory (0.38→0.58 over 10 years) reflects industry shift from content-centric to data-monetization models. Suppression (0.62): High. Significant barriers prevent subscriber exit: platform switching loses content continuity and communities, alternative newsletters use identical consent patterns, explicit data flows are hidden behind technical opacity and legal jargon. But suppression is not absolute (≤1.0) because regulatory pressure is creating cracks: privacy regulations provide legal grounds for resistance, browser vendors are implementing tracking restrictions, some subscribers do successfully minimize exposure. Theater ratio (0.55): Moderate-high. Consent mechanisms (cookie banners, privacy policies) are significantly performative: they create legal cover rather than informed choice, satisfy regulatory checkboxes without enabling actual user control, use dark patterns and opacity to manufacture consent. Theater has increased over the interval as compliance theater has become more sophisticated. The measured trajectory (0.35→0.55) shows rising performativity as regulatory pressure increases the need for legal theater.
 *
 * PERSPECTIVAL GAP:
 *   This constraint's perspectival gap is unusually wide: the snare (subscriber view) and rope (publisher view) perspectives represent nearly opposite structural experiences of identical machinery. A trapped subscriber sees pure extraction because their data is monetized without meaningful return; a publisher sees coordination because the subscription list solves operational problems (audience tracking, engagement metrics, payment processing). The analytical observer captures both: yes, subscription infrastructure coordinates content delivery (rope function), and yes, data extraction asymmetrically benefits publishers and ad networks (snare outcome for subscribers). The gap is not an error in classification—it reveals the true hybrid structure. Piton classification for consent theater reflects that performative compliance has largely replaced functional informed choice: the consent banner ritual persists through legal requirement and institutional inertia, but its stated purpose (actual subscriber understanding and control) has atrophied.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) encode each agent's structural relationship to extraction flow. Subscribers (powerless/trapped) derive high d (~0.95) because they bear full extraction cost with no exit capacity. Privacy-aware subscribers (moderate/constrained) derive moderate d (~0.55) because they have constrained exit options and experience mixed value (personalization benefit moderates pure extraction). Publishers and ad networks (institutional/powerful + arbitrage) derive low d (~0.15-0.20) because they're beneficiaries with high exit optionality. The sigmoid f(d) maps these d values to experienced extractiveness: high d produces high f(d) (~1.28-1.42 for trapped/powerless agents), amplifying their experienced extraction; low d produces low f(d) (~-0.01-0.02 for beneficiaries), making their experience of the constraint resemble pure coordination. The scope modifier σ(S) at global scale (σ=1.2) amplifies χ for powerless agents (harder to detect and resist data flows at planetary scale) while amplifying it slightly for powerful agents (easier to coordinate globally). The tangled rope classification reflects both the coordination function (subscription infrastructure genuinely enables content delivery) and the asymmetric extraction (data flows primarily benefit ad networks and publishers, not subscribers).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that newsletter subscription is genuinely a tangled rope at system level but creates a snare outcome for powerless trapped agents. The coordination function is real: subscription data enables personalization, engagement metrics, and content distribution infrastructure. But this coordination is coupled with asymmetric extraction that flows overwhelmingly toward publishers and ad networks. The snare classification (subscriber view) is not wrong—it's the lived experience of an agent with no exit. The rope classification (publisher view) is also not wrong—it's their genuine coordination need. The tangled rope classification at the analytical level captures both. The mandatrophy prevention mechanism here is refusing to call the constraint 'just coordination' (pure rope) when the structural asymmetry is visible from powerless perspectives. Equally, refusing to call it 'just extraction' (pure snare) when coordination functions genuinely exist. The theater ratio (0.55) prevents false piton classification: consent mechanisms are performative enough to flag degradation but not theatrical enough (not ≥0.70) to indicate atrophied coordination. Instead, theater indicates that the coordination function is obscured by compliance theater rather than absent. As privacy regulations take effect (scaffold sunset), theater may rise (compliance performance theater) while extractiveness drops (actual data extraction mechanisms decline), potentially reclassifying to piton if regulations succeed but coordination infrastructure atrophies.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    personalization_boundary,
    'What proportion of data collection is necessary for content personalization vs. extractive behavioral profiling?',
    'A/B testing: personalized experience with minimal data (first-party only, no cross-domain tracking) vs. status quo. Measurement of content relevance impact and engagement metrics.',
    'If minimal data suffices: base_extractiveness drops to 0.35 (pure coordination), snare perspectives reclassify as rope. If current data volume is necessary: extraction margin is smaller than analyzed, extractiveness drops to 0.48 but remains above snare threshold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(personalization_boundary, empirical, 'Minimum data requirement for content personalization').

omega_variable(
    suppression_mechanism_internalization,
    'Is measured suppression (0.62) structural (opaque technology, legal barriers) or internalized (subscribers believe tracking is inevitable)?',
    'Post-awareness measurement: survey subscribers after transparent data audit showing exact data flows and third-party recipients. Does perceived suppression drop when architecture is visible? Do exit rates increase after full information?',
    'If internalized (>50%): suppression persists after barrier removal; subscriber exit requires identity shift (reclassify exit_options from ''trapped'' to ''identity_locked''). If structural: suppression declines post-audit; exit becomes meaningful. Determines whether constraint binds through technology or belief.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in subscriber behavior').

omega_variable(
    regulatory_sunset_realism,
    'Will privacy regulations (GDPR, US state laws, cookie deprecation) actually reduce extraction, or will industry shift to alternative tracking mechanisms (server-side tracking, fingerprinting, consent-washing)?',
    'Longitudinal measurement: compare data extraction rates before/after regulatory implementation; measure adoption of alternative tracking technologies; track consent compliance rates vs. actual data flows.',
    'If regulations effective: scaffold sunset is real, extractiveness declines post-2025. If regulations circumvented: constraint persists under new technical form, extractiveness drops marginally (0.58→0.54), but classification remains tangled_rope. Theater ratio may rise (0.55→0.68) as compliance performance becomes more theatrical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_sunset_realism, empirical, 'Effectiveness of privacy regulations in reducing extraction vs. shifting mechanisms').

omega_variable(
    consent_dark_patterns_quantification,
    'What is the opt-in rate for data sharing under dark pattern design vs. neutral presentation?',
    'Experimental: present consent dialog to equivalent subscriber groups with (a) dark patterns (default on, small gray reject button), (b) neutral design (equal prominence, balanced language). Measure opt-in rates and actual data sharing alignment with stated preferences.',
    'If dark patterns cause >60% consent rate shift: theater_ratio increases (0.55→0.72), piton reclassification strengthened. If effect is minor: consent mechanisms have some genuine agency, piton drops to snare. Suppression component clarified as intentional design vs. incidental opacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_dark_patterns_quantification, empirical, 'Dark pattern impact on consent rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(newsletter_subscriber_privacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nletter_tr_t0, newsletter_subscriber_privacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nletter_tr_t3, newsletter_subscriber_privacy, theater_ratio, 3, 0.42).
narrative_ontology:measurement(nletter_tr_t6, newsletter_subscriber_privacy, theater_ratio, 6, 0.5).
narrative_ontology:measurement(nletter_tr_t10, newsletter_subscriber_privacy, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(nletter_be_t0, newsletter_subscriber_privacy, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nletter_be_t3, newsletter_subscriber_privacy, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(nletter_be_t6, newsletter_subscriber_privacy, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(nletter_be_t10, newsletter_subscriber_privacy, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(newsletter_subscriber_privacy, resource_allocation).
narrative_ontology:boltzmann_floor_override(newsletter_subscriber_privacy, 0.18).
narrative_ontology:affects_constraint(newsletter_subscriber_privacy, advertising_attention_extraction).
narrative_ontology:affects_constraint(newsletter_subscriber_privacy, dark_pattern_behavioral_engineering).
narrative_ontology:affects_constraint(newsletter_subscriber_privacy, platform_algorithmic_ranking_bias).

% DUAL FORMULATION NOTE:
% Newsletter privacy is upstream of platform-level privacy extraction dynamics. While newsletter subscription operates at the individual content-delivery level, the constraint family includes platform-wide attention extraction (advertising networks), behavioral engineering through interface design (dark patterns), and algorithmic ranking bias (recommendation system asymmetries). All three are downstream of newsletter subscriber data collection. The newsletter story focuses on direct subscriber-publisher-advertiser dynamics; platform stories examine systemic amplification of extraction across multiple content types.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(newsletter_subscriber_privacy, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
