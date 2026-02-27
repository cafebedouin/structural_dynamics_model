% ============================================================================
% CONSTRAINT STORY: beehiiv_platform_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_beehiiv_platform_model, []).

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
 *   constraint_id: beehiiv_platform_model
 *   human_readable: The Beehiiv Newsletter Platform Business Model
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Beehiiv's platform model combines genuine coordination (solving
 *   newsletter distribution and monetization for creators) with significant
 *   asymmetric extraction (data collection, feature gatekeeping, revenue
 *   sharing, platform lock-in). The constraint exhibits a classic tangled
 *   rope structure: creators benefit from tools and audience discovery, but
 *   Beehiiv captures disproportionate value through lock-in mechanisms,
 *   behavioral data aggregation, and control of the sponsorship marketplace.
 *   The theater ratio (0.61) reflects performative features: engagement
 *   metrics, growth loops, and algorithmic recommendation that create the
 *   appearance of meritocratic discovery while actually concentrating
 *   visibility and income to high-performing early adopters. Extractiveness
 *   has risen from 0.28 (early platform, genuine coordination) to 0.52
 *   (mature platform, rent-seeking via data and lock-in) over six years as
 *   the company transitioned from creator service to data-driven
 *   intermediary. The perspectival gap is wide: small creators experience
 *   pure extraction (snare), established creators experience mixed
 *   coordination-extraction (tangled rope), Beehiiv experiences coordination
 *   (rope), and the analytical observer risks naturalizing platform mediation
 *   as inherent to creator economics.
 *
 * KEY AGENTS:
 *   - Beehiiv Company: Primary beneficiary (institutional/arbitrage) — captures value via platform fees, data monetization, and lock-in effects; experiences constraint as coordination
 *   - Small Creators: Primary victims (powerless/trapped) — depend on platform for audience growth; locked in by switching costs; experience extraction without meaningful exit
 *   - Subscriber Base: Secondary victims (powerless/trapped) — provide behavioral data through platform collection; no opt-out or meaningful consent
 *   - Established Creators: Mixed position (powerful/mobile) — large audiences provide exit options; experience both benefit and extraction; can negotiate terms
 *   - Open Protocol Coalition: Organized challengers (organized/constrained) — building alternative decentralized platforms; see extraction as temporary with sunset via protocol migration
 *   - Legacy Email Systems: Institutional degradation (institutional/arbitrage) — replaced by platform intermediation; their discovery role atrophied; persist through inertia only
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(beehiiv_platform_model, 0.52).
domain_priors:suppression_score(beehiiv_platform_model, 0.58).
domain_priors:theater_ratio(beehiiv_platform_model, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(beehiiv_platform_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(beehiiv_platform_model, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(beehiiv_platform_model, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(beehiiv_platform_model, tangled_rope).
narrative_ontology:human_readable(beehiiv_platform_model, "The Beehiiv Newsletter Platform Business Model").
narrative_ontology:topic_domain(beehiiv_platform_model, "technological/economic").

domain_priors:requires_active_enforcement(beehiiv_platform_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, beehiiv_company).
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, platform_early_adopters).
narrative_ontology:constraint_beneficiary(beehiiv_platform_model, high_performing_creators).
narrative_ontology:constraint_victim(beehiiv_platform_model, creator_autonomy).
narrative_ontology:constraint_victim(beehiiv_platform_model, subscriber_data_commons).
narrative_ontology:constraint_victim(beehiiv_platform_model, nascent_creator_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CREATOR (TRAPPED) — Small creators pursuing audience growth become dependent on Beehiiv's algorithmic recommendation system, audience insights, and monetization features. Once invested (followers, archive, payment infrastructure), switching platforms incurs high switching costs. The platform's growth tools create lock-in: the more a creator uses recommendations and audience data, the higher the extraction cost of departure. This creator experiences the constraint as pure extraction with no viable exit.
constraint_indexing:constraint_classification(beehiiv_platform_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUBSCRIBER DATA (TRAPPED COLLECTIVE) — Subscribers unknowingly participate in data aggregation: Beehiiv collects reading behavior, engagement patterns, and email metadata across all newsletters on the platform. Subscribers have no opt-out for data collection (only creator-controlled disclosure) and no direct exit from the data commons once they engage with any newsletter on the platform. The constraint extracts behavioral data with minimal transparency. Data becomes a public good Beehiiv privately captures.
constraint_indexing:constraint_classification(beehiiv_platform_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: ESTABLISHED CREATOR (MOBILE) — High-performing creators with large followings experience the platform as mixed coordination and extraction. Beehiiv's tools (growth loops, subscriber insights, sponsorship matching) genuinely enable their success. But the platform also extracts via revenue share (affiliate commissions on sponsorships, Beehiiv's fee structure), terms-of-service enforcement, and feature gatekeeping. These creators have partial exit: their audience is portable, but migration costs are real. They negotiate rather than accept; they have leverage. They experience both benefit and extraction.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: BEEHIIV COMPANY (BENEFICIARY) — The platform benefits from network effects and switching costs. Early creators establish network value; later creators find existing audience pools; Beehiiv captures value through fees, data, and platform lockage. From Beehiiv's perspective, the constraint is coordination: they are solving the legitimate problem of newsletter distribution and creator monetization. The extraction mechanisms (data collection, revenue share, feature gatekeeping) appear as fair value capture for infrastructure. This institutional actor experiences the constraint as rope — low-extraction coordination.
constraint_indexing:constraint_classification(beehiiv_platform_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN CREATOR COALITION (CONSTRAINED) — Organized creators and indie hackers see the Beehiiv model as a coordination failure fixable via open protocols (ActivityPub-style decentralized email newsletters, open subscriber directories, interoperable audience data). This coalition views the platform lock-in and data extraction as temporary — sunset logic applies if creators adopt open-source alternatives (Ghost, Substack open-source derivatives). They experience extraction but see an exit path via protocol migration. Sunset estimated at 5-10 years as interoperable platforms mature.
constraint_indexing:constraint_classification(beehiiv_platform_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY EMAIL NEWSLETTER INFRASTRUCTURE (PITON) — From a civilizational view, Beehiiv's constraint operates atop (and partially replaces) the legacy email newsletter ecosystem: flat email list providers (Mailchimp), RSS feeds, and creator-owned domain-based subscriptions. This legacy system persists through institutional inertia despite reduced functionality. Beehiiv's performative features (engagement tracking, growth loops) add theater — creators measure success by platform metrics rather than subscriber relationship quality. The old ecosystem sees its own degradation as Beehiiv captures the growth incentive layer. Piton classification derives from high theater (0.61) and the replacement of functional (direct email) with platform-mediated (algorithmic recommendation) verification.
constraint_indexing:constraint_classification(beehiiv_platform_model, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (UNIVERSAL FRAME) — From a universal/analytical perspective, there is a natural structural limit to decentralized creator economies: audience discovery requires intermediation. Without a platform or algorithm matching creators to subscribers, discovery becomes a broadcast problem solvable only via existing social graphs or paid advertising. The 'natural law' claim is that some intermediary must capture value to fund discovery. However, this perspective risks false summitry — it naturalizes Beehiiv's specific rent-extraction model as inevitable, when alternative discovery mechanisms (creator collectives, curated directories, subscriber-driven recommendations) exist and require less extraction.
constraint_indexing:constraint_classification(beehiiv_platform_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(beehiiv_platform_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(beehiiv_platform_model, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(beehiiv_platform_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(beehiiv_platform_model, TR),
    TR >= 0.70.

:- end_tests(beehiiv_platform_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52): Moderate-high. Beehiiv extracts through multiple mechanisms: platform fee (10% of subscription revenue for paid tiers), data collection and algorithmic control of audience discovery, and gatekeeping of premium features (sponsorship matching, advanced analytics). The extraction is not total (creators retain subscriber relationships and can theoretically migrate) but substantial (switching costs are high, algorithmic reach creates dependency). The value of Beehiiv's coordination (solving discovery and monetization) justifies some extraction, but the ratio has skewed toward rent-seeking as the platform matured. Early Beehiiv (ε=0.28) genuinely solved creator infrastructure gaps; current Beehiiv (ε=0.52) prioritizes network lock-in and data monetization. Suppression (0.58): Moderate-high. Significant barriers to creator exit include switching costs (migrating subscriber list, rebuilding growth metrics, learning new platform), algorithmic dependency (recommendation system drives disproportionate audience growth for platform users), and feature asymmetry (Beehiiv's sponsorship matching is unavailable on alternatives). Data extraction suppresses subscriber exit via opaque collection and lack of portability. Theater ratio (0.61): Moderate-high. Engagement metrics and growth loops serve dual purposes: genuine creator feedback (functional) and gamification encouraging platform investment (performative). Algorithmic recommendations appear meritocratic but actually concentrate visibility on high-performing early adopters and Beehiiv-promoted creators. Sponsored content matching algorithm is opaque to creators — they see recommended sponsorships but not the ranking logic. Premium analytics create theater: creators obsess over platform-specific metrics rather than sustainable audience relationships.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates perspectival divergence across power and exit dimensions. The small creator (powerless/trapped) sees a snare: extraction without alternatives, platform dependency, and opaque algorithmic control. The established creator (powerful/mobile) sees tangled rope: genuine coordination (audience discovery, monetization tools) mixed with extraction (revenue share, feature gatekeeping), but with leverage to negotiate. Beehiiv (institutional/arbitrage) sees rope: solving the legitimate creator infrastructure problem, with value capture as fair compensation. The open coalition (organized/constrained) sees tangled rope with sunset: extraction is real but alternative protocols are emerging, timeline estimated 5-10 years. The legacy email system sees piton: its functional role (email delivery) is intact, but Beehiiv's intermediary layer has captured audience discovery and monetization, leaving the legacy system as degraded infrastructure persisting through institutional inertia. The analytical observer risks mountain: 'creators always need discovery intermediaries; Beehiiv's extraction is therefore inevitable.' This false summit naturalizes a contingent institutional choice (algorithmic platform mediation) as natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position and exit options. Beehiiv benefits from lock-in and data capture (d ≈ 0.05, full beneficiary, negative effective extraction); small creators bear costs of dependency without exit (d ≈ 0.95, full target, high effective extraction); established creators have partial exit and partial benefit (d ≈ 0.55, symmetric, moderate effective extraction). Subscribers are trapped in data collection with no direct choice (d ≈ 0.90, high extraction target). The open coalition has clear exit mechanisms emerging (d ≈ 0.40, victim but with organizational capacity and alternative pathways). The engine derives these d values from beneficiary/victim declarations and exit_options tuples, producing the perspectival gap observed above.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: This constraint resolves the mandatrophy by showing that 'is Beehiiv extraction or coordination?' is the wrong question — it is structurally BOTH. The platform genuinely solved a creator infrastructure gap (coordination function) AND extracted asymmetric value via lock-in and data capture (extraction mechanism). The mandatrophy is resolved by: (1) Declaring beneficiaries (Beehiiv, early adopters, high performers) — these are the coordination beneficiaries. (2) Declaring victims (creator autonomy, subscriber privacy, nascent competition) — these are the extraction targets. (3) Setting requires_active_enforcement=true — the lock-in and algorithmic control require continuous platform enforcement to maintain. (4) Measuring the perspectival gap — snare vs rope vs tangled rope vs piton across different agents confirms the hybrid nature. The open coalition's scaffold perspective confirms that the extraction is not inherent: alternative mechanisms (open protocols, community discovery, subscriber-driven curation) exist with lower extraction ratios, and their adoption would sunset Beehiiv's current model. This means the extraction was contingent on platform dominance, not natural. The false summit risk (analytical mountain perspective) is flagged: 'creators need discovery intermediaries' is true, but 'those intermediaries must extract via lock-in and opaque data collection' is false — viable alternatives exist with lower extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    discovery_mechanism_necessity,
    'Is algorithmic audience discovery a natural bottleneck (requiring platform intermediation) or a contingent design choice?',
    'Comparative analysis of discovery mechanisms: algorithmic recommendation vs creator directories, subscriber-driven discovery, editorial curation, community-based matching. Measurement of creator growth rates and audience reach under each mechanism.',
    'If algorithmic discovery is necessary: Beehiiv''s extraction is justified coordination overhead (shifts toward Rope). If contingent: alternative mechanisms exist with lower extraction (shifts toward Snare/Tangled Rope clarification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(discovery_mechanism_necessity, empirical, 'Whether algorithmic discovery is inherent or contingent').

omega_variable(
    data_portability_feasibility,
    'Can subscriber data and engagement metrics be truly ported to competing platforms without loss of functionality or creator relationship continuity?',
    'Technical audit of Beehiiv export formats vs. open standards. User study tracking creator migration friction and subscriber churn during platform transitions.',
    'If portable: switching costs are lower than perceived, exit_options upgrade from trapped to constrained/mobile for established creators (changes perspectival gap). If non-portable: lock-in is real, extraction is higher.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_portability_feasibility, empirical, 'Whether subscriber data can be ported across platforms').

omega_variable(
    open_protocol_viability_timeline,
    'What is the realistic adoption timeline for open-source newsletter platforms (Ghost, interoperable ActivityPub email systems) as direct Beehiiv competitors?',
    'Tracking creator adoption of self-hosted and open-source alternatives. Analysis of feature parity and total cost of ownership vs. Beehiiv. Community funding and development velocity for open-source projects.',
    'If timeline < 5 years: scaffold sunset logic is structural, not aspirational. If timeline > 15 years: open coalition perspective is optimistic, extraction persists longer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_protocol_viability_timeline, empirical, 'Adoption timeline for open-protocol alternatives').

omega_variable(
    subscriber_consent_interpretation,
    'Does creator-disclosed data collection constitute genuine subscriber consent or obfuscated platform data harvesting?',
    'Analysis of disclosure frequency and comprehensibility in creator communication. Survey of subscribers'' understanding of data collection scope. Comparison of disclosed vs. actual data fields collected.',
    'If genuine consent: data extraction is coordinated harm reduction (shifts toward Rope). If obfuscated: data extraction is non-consensual (shifts toward Snare confirmation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subscriber_consent_interpretation, conceptual, 'Whether subscriber data collection constitutes genuine consent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(beehiiv_platform_model, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beehiiv_tr_t0, beehiiv_platform_model, theater_ratio, 0, 0.38).
narrative_ontology:measurement(beehiiv_tr_t3, beehiiv_platform_model, theater_ratio, 3, 0.5).
narrative_ontology:measurement(beehiiv_tr_t6, beehiiv_platform_model, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(beehiiv_be_t0, beehiiv_platform_model, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(beehiiv_be_t3, beehiiv_platform_model, base_extractiveness, 3, 0.4).
narrative_ontology:measurement(beehiiv_be_t6, beehiiv_platform_model, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(beehiiv_platform_model, resource_allocation).
narrative_ontology:affects_constraint(beehiiv_platform_model, email_deliverability_bottleneck).
narrative_ontology:affects_constraint(beehiiv_platform_model, creator_income_concentration).
narrative_ontology:affects_constraint(beehiiv_platform_model, newsletter_subscriber_privacy).

% DUAL FORMULATION NOTE:
% The Beehiiv platform model decomposes into three structurally distinct constraints: (1) Resource allocation coordination (who gets discovery and monetization infrastructure) — drives the tangled rope classification. (2) Email deliverability as a natural bottleneck (spam filtering, ISP throttling) — constrains all newsletter platforms equally. (3) Subscriber data extraction and privacy (behavioral tracking, profile aggregation) — a separate extractive mechanism. The current story (beehiiv_platform_model) captures the resource allocation hybrid. The email deliverability bottleneck is a separate mountain/rope constraint. The subscriber privacy extraction is a separate snare constraint. These three stories are linked via affects_constraints because Beehiiv's resource allocation model depends on email deliverability infrastructure and exploits the privacy bottleneck.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(beehiiv_platform_model, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
