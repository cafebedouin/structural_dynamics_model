% ============================================================================
% CONSTRAINT STORY: platform_app_store_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_app_store_duopoly, []).

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
 *   constraint_id: platform_app_store_duopoly
 *   human_readable: Platform Mandate for Proprietary App Stores and In-App Payments
 *   domain: technological/platform_economics
 *
 * SUMMARY:
 *   The app store mandate in iOS and Android represents a structural
 *   constraint where two platform operators (Apple and Google) require all
 *   third-party app distribution and in-app payments to flow through
 *   proprietary systems charging 15-30% commissions. This constraint exhibits
 *   a genuine tangled hybrid: the app stores solve real coordination problems
 *   (trusted distribution, payment processing, fraud prevention, developer
 *   onboarding) while simultaneously extracting economic rent through market
 *   power and suppressing alternative distribution channels. The constraint's
 *   classification varies dramatically by observer: dependent developers
 *   experience it as a snare with no exit; platform operators experience it
 *   as pure coordination infrastructure; large organized developers
 *   experience it as a tangled hybrid they can partially negotiate;
 *   regulators see it as theater masking extraction behind consumer
 *   protection narratives. The increasing extractiveness trajectory
 *   (0.35→0.58 over the interval) reflects platform operators' increasing
 *   confidence to enforce stricter policies (rejection of alternative payment
 *   methods, commission increases, unpredictable guideline changes) as
 *   regulatory risk was perceived as low. The decreasing theater ratio
 *   (0.55→0.48) reflects that the curation justification has become less
 *   credible as evidence accumulates that app review is inconsistently
 *   applied, first-party apps bypass review, and sideloading security data
 *   contradicts necessity claims.
 *
 * KEY AGENTS:
 *   - Apple Inc. and Google LLC: Primary beneficiary operators (institutional/arbitrage) — capture 15-30% commission on all third-party in-app purchases; can modify terms unilaterally; exempt own apps from commission rules
 *   - Third-Party App Developers (small/medium): Primary victims (powerless/trapped) — dependent on app store distribution with no alternative; pay full commission; subject to arbitrary guideline enforcement
 *   - Developer Coalition (Epic Games, Spotify, Basecamp): Organized victims (organized/constrained) — attempting collective action through lawsuits, regulatory lobbying; constrained by coalition maintenance costs and retaliation risk
 *   - Large Incumbent Developers (Netflix, Microsoft, Amazon): Secondary victims (powerful/mobile) — can negotiate better rates or build workarounds; constrained by need to maintain mobile presence; have alternative channels
 *   - First-Party Apps (Apple Music, Google Maps, YouTube): Primary beneficiary applications (institutional/arbitrage) — exempt from commission rules; benefit from privileged platform access; experience constraint as pure rope
 *   - App Store Infrastructure: System function (institutional/arbitrage) — payment processing, fraud prevention, developer onboarding; experiences constraint as coordination solving genuine problems
 *   - Consumers: Secondary victims (moderate/constrained) — pay higher prices due to commission pass-through; limited access to alternative payment methods; constrained by platform app availability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_app_store_duopoly, 0.58).
domain_priors:suppression_score(platform_app_store_duopoly, 0.72).
domain_priors:theater_ratio(platform_app_store_duopoly, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_app_store_duopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_app_store_duopoly, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(platform_app_store_duopoly, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_app_store_duopoly, tangled_rope).
narrative_ontology:human_readable(platform_app_store_duopoly, "Platform Mandate for Proprietary App Stores and In-App Payments").
narrative_ontology:topic_domain(platform_app_store_duopoly, "technological/platform_economics").

domain_priors:requires_active_enforcement(platform_app_store_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, apple_google_platform_operators).
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, first_party_app_developers).
narrative_ontology:constraint_victim(platform_app_store_duopoly, third_party_app_developers).
narrative_ontology:constraint_victim(platform_app_store_duopoly, app_distribution_ecosystem).
narrative_ontology:constraint_victim(platform_app_store_duopoly, consumer_payment_alternatives).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT APP DEVELOPER (SNARE) — A third-party developer with no meaningful alternative distribution channel. Cannot exit without abandoning market access to billions of users. Pays 15-30% commission on all in-app purchases, subscription revenue, and indirect monetization. Zero bargaining power. Suppression is absolute: App Store Review Guidelines are non-negotiable, terms of service unilateral, enforcement arbitrary. Maximum experienced extraction.
constraint_indexing:constraint_classification(platform_app_store_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPER COALITION (TANGLED ROPE) — Collective of organized developers (Epic Games, Spotify, Basecamp, etc.) who recognize the constraint and attempt collective action. Constrained by the coordination cost of maintaining coalition (antitrust lawsuits, regulatory lobbying) and the platform's ability to retaliate (app demotion, rejection). Benefits from the ecosystem (distribution infrastructure, payment processing, fraud prevention) but also bears extraction through commissions and policy changes. Active enforcement of app removal creates suppression; coordination function (payment infrastructure, fraud protection) creates partial rope component.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR - FIRST-PARTY APPS (ROPE) — Apple's and Google's own apps (Apple Music, Google Maps, YouTube, iCloud+, Google Drive, Google Photos) are exempt from the 15-30% commission on in-app purchases or pay themselves zero percent. These actors experience the app store mandate as pure coordination: the store infrastructure solves distribution and payment processing. Zero extraction from this perspective; net beneficiary of the coordination mechanism. Arbitrage exit option reflects ability to change policy unilaterally.
constraint_indexing:constraint_classification(platform_app_store_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE INCUMBENT DEVELOPER (TANGLED ROPE) — Billion-dollar companies (Spotify, Netflix, Microsoft, Amazon, Discord) with diversified monetization and alternative distribution channels (web, desktop, direct subscriptions). Mobile/exit options more favorable than dependent developers. Constrained by the need to maintain mobile presence (13+ billion users accessible nowhere else) but can negotiate special rates, build workarounds (external subscription links), or invest in regulatory challenges. Experiences the constraint as extraction (commissions, policy changes) but also benefits from the app store's distribution infrastructure and fraud prevention. Organized power allows coalition-building; neither fully trapped nor fully arbitrage.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM OPERATOR - SYSTEM FUNCTION (ROPE) — From the perspective of the platform operator's own apparatus (Apple Inc., Google LLC), the app store mandate solves genuine coordination problems: code signing, trusted distribution, payment processing, developer onboarding, fraud prevention, consumer refund mediation. The 15-30% commission reflects real system costs plus network effects value. Experienced as pure coordination infrastructure. Zero experienced extraction from this structural position.
constraint_indexing:constraint_classification(platform_app_store_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY THEATER (PITON) — The stated justification for exclusive app stores is content curation and consumer protection against malware. This theatrical element persists despite being largely decoupled from actual function: (a) app review is not real-time verification against exploit databases; (b) many app store rejections cite 'guideline violations' unprovable to objective standards; (c) sideloading on Android allows arbitrary installation, proving the curation claim is not about technical necessity; (d) first-party apps bypass review processes. Theater ratio (0.48) reflects that some real quality control exists, but much of the enforcement narrative serves rent-extraction justification. The curation rationale is maintained through institutional inertia and regulatory compliance theater.
constraint_indexing:constraint_classification(platform_app_store_duopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective unbounded by any single actor's constraints, the app store system exhibits genuine coordination function (trusted distribution, payment processing, fraud prevention) AND asymmetric extraction (15-30% commissions on dependent developers, policy rent-seeking, restriction of alternative distribution). The mandatrophy is resolved by recognizing this as a real hybrid: the system solves a real problem while generating rents. Not pure Snare (the infrastructure genuinely helps developers), not pure Rope (the extraction is not symmetric or consensual). The analytical classification is Tangled Rope from all structurally neutral viewpoints.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_app_store_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_app_store_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_app_store_duopoly, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_app_store_duopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_app_store_duopoly, TR),
    TR >= 0.70.

:- end_tests(platform_app_store_duopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. The 15-30% commission extracted from dependent developers represents significant economic rent. Base extraction increased from 0.35 to 0.58 over the interval as platforms became more aggressive in enforcing exclusive payment methods and raising commissions. However, the value reflects that not all commission is pure rent — payment processing infrastructure, fraud prevention, and distribution value constitute genuine coordination cost (approximately 5-12% based on competitive benchmarks like Stripe at 2.3%). The remainder (8-25%) represents extraction through market power. Suppression (0.72): High. Multiple suppression mechanisms: (1) technical control (code signing, app review, certificate authority) makes sideloading difficult on iOS; (2) policy enforcement (App Store Review Guidelines, arbitrary rejections); (3) economic barriers (network effects make 1.5B+ user base inaccessible without platform); (4) legal risk (developers fear retaliation via app removal or demotion); (5) information asymmetry (review criteria opaque, guidelines change without notice). Theater ratio (0.48): Moderate. Consumer protection and malware prevention are genuine but not the primary enforcement mechanism. Theater element: (a) first-party apps bypass review entirely (proving curation is not about security necessity); (b) review criteria are inconsistently applied across different app categories; (c) sideloading on Android allows arbitrary installation with lower harm rates than narrative suggests; (d) review processes are delayed and unpredictable rather than evidence-based; (e) many rejections cite 'guideline violations' without objective verification standards. Theater decreased from 0.55 to 0.48 as regulatory scrutiny (DMA, US antitrust) forced platforms to articulate clearer review standards, increasing functional transparency. Claimed type (Tangled Rope): The constraint is not pure Snare because genuine coordination value exists (payment processing, fraud prevention, developer discovery tools, consumer refund mediation). It is not pure Rope because the extraction is severe and the asymmetry is extreme. The active enforcement requirement (required for Tangled Rope gate) is satisfied: platforms actively enforce exclusive payment methods, reject competing payment solutions, and threaten app removal for policy violations.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why classification is observer-relative. The dependent developer's Snare classification (powerless/trapped) reflects their lived experience: no meaningful alternative exists; commissions are non-negotiable; policy changes are unilateral; retaliation risk is real. The platform operator's Rope classification (institutional/arbitrage) reflects their structural position: app store infrastructure solves genuine problems (payment, fraud, discovery); they have full control over implementation; first-party apps are exempt; margins subsidize their operating costs. The developer coalition's Tangled Rope classification (organized/constrained) reflects their intermediate position: they have collective action capacity (lawsuits, regulatory pressure) but at high coordination cost; they benefit from the ecosystem (1.5B users accessible nowhere else) but pay extraction; they can partially negotiate better terms but cannot unilaterally change the system. The analytical observer's classification is also Tangled Rope (civilizational/analytical): neither pure coordination nor pure extraction — the system solves real problems while generating unwarranted rents. The perspectival gap reveals that the constraint's nature depends on the observer's exit options: those with exits see coordination; those without see extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) derive from the structural position of each agent relative to the extraction flow. Beneficiary agents (platform operators, first-party apps) have low d values (0.05-0.20): they are net recipients of the commission flow and have exit options (arbitrage — ability to change policy unilaterally). Victim agents (dependent developers) have high d values (0.90-0.95): they are net payers of commissions with no functional exit. Organized developers have intermediate d values (0.55-0.65): they have some exit options (regulatory challenges, coalition pressure, building workarounds) and can partially negotiate, but constrained by the network effect lock-in. The sigmoid f(d) transforms these directionality values into experienced extractiveness multipliers. A powerless agent with d=0.95 experiences f(d)≈1.42, making the nominal 15-30% commission feel like severe extraction. An institutional agent with d=0.05 experiences f(d)≈-0.12, making the commission structure appear as neutral coordination. An organized developer with d=0.60 experiences f(d)≈0.85, experiencing moderate extraction with some negotiation space.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by the tangled rope classification itself. The constraint does NOT reduce to pure coordination (Rope) because: (1) the 15-30% commission is dramatically higher than competitive payment processing costs (2-3%); (2) first-party apps are exempt, creating asymmetry; (3) the extraction cannot be renegotiated or exited by dependent developers; (4) suppression (policy enforcement, guideline arbitrariness) serves to maintain extraction, not solve coordination. The constraint does NOT reduce to pure extraction (Snare) because: (1) the app store infrastructure provides genuine value (payment processing, fraud prevention, developer discovery, consumer refund mediation); (2) organized developers can partially negotiate better terms and build workarounds; (3) the system facilitates legitimate entrepreneurship — many successful businesses exist within these constraints; (4) the coordination function is not a pretext but a real technical requirement. The resolution is that this is a legitimate Tangled Rope: a system that solves a genuine coordination problem (how to distribute trusted applications to billions of users, process payments securely, prevent fraud) while simultaneously extracting economic rent through market power. The mandatrophy risk would be claiming this is pure Rope (erasing the extraction and asymmetry) or pure Snare (erasing the coordination value). Tangled Rope classification preserves both structural truths.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commission_necessity_threshold,
    'What portion of the 15-30% commission is necessary payment for genuine platform services (payment processing, fraud prevention, distribution infrastructure) versus economic rent extracted through market power?',
    'Cost accounting analysis of Apple/Google app store operations (infrastructure, personnel, fraud prevention); comparison to competitive payment processing rates (2-3% for Stripe, PayPal); benchmarking against alternative app distribution models (F-Droid, Samsung Galaxy Store, Epic Games Launcher)',
    'If 5-10% necessary, 5-20% rent: extraction classification strengthened (Snare from more perspectives). If 12-15% necessary, 0-5% rent: coordination classification strengthened (Rope from more perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commission_necessity_threshold, empirical, 'What portion of commission is necessary versus rent').

omega_variable(
    genuine_curation_necessity,
    'Is centralized app store curation technically necessary for malware prevention and consumer protection, or is it a social/regulatory choice decoupled from technical necessity?',
    'Security analysis comparing curated app stores to sideloading markets (Android, PC); malware prevalence rates; consumer harm statistics; technical feasibility of decentralized security models (reputation systems, code signing verification, crowdsourced review)',
    'If technically necessary: coordination function justified (higher Rope classification). If decoupled from necessity: curation is regulatory theater (Piton/Snare classification strengthened).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(genuine_curation_necessity, empirical, 'Whether app curation is technically necessary or regulatory theater').

omega_variable(
    coalition_critical_mass,
    'At what developer coalition size does collective action become viable for negotiating alternative payment terms or regulatory change?',
    'Game-theoretic analysis of defection incentives; empirical tracking of developer coalition sentiment (DMA regulations, recent lawsuits, antitrust referrals); threshold modeling for legal/regulatory pressure',
    'If large coalitions (100+ companies, $100B+ revenue) remain powerless: Snare perspective dominates. If coalition threshold is reachable: developer organizing can shift classification to sustained Tangled Rope negotiation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_critical_mass, empirical, 'Critical mass for viable developer collective action').

omega_variable(
    alternative_distribution_viability,
    'Can alternative app distribution channels (web progressive apps, desktop app stores, direct sideloading) become functionally equivalent to mobile app stores for consumer reach and developer revenue?',
    'Longitudinal tracking of progressive web app adoption, desktop app store growth (Epic Games Launcher, Microsoft Store, Steam), sideloading prevalence on Android; consumer willingness-to-pay and monetization sustainability on alternative channels',
    'If viable alternatives emerge: exit options shift from ''trapped'' to ''mobile/constrained'' for more developers, reducing Snare severity. If barriers remain insurmountable: Snare classification persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_distribution_viability, empirical, 'Whether alternative distribution channels can replicate mobile reach').

omega_variable(
    regulatory_mandate_sustainability,
    'Will DMA/Digital Markets Act regulatory interventions (forced sideloading, third-party payment methods, fair access to app store) survive implementation without platform evasion or legislative rollback?',
    'Monitoring of EU DMA compliance mechanisms, US antitrust enforcement, developer regulatory capture risk, platform lobbying effectiveness, sunset/expiration of regulatory mandates',
    'If regulations are sustained and enforced: constraint shifts toward Scaffold (temporary enforcement with sunset). If platforms evade or regulations are rolled back: constraint persists as Tangled Rope/Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mandate_sustainability, empirical, 'Sustainability of regulatory interventions against app store mandates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_app_store_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(appstore_tr_t0, platform_app_store_duopoly, theater_ratio, 0, 0.55).
narrative_ontology:measurement(appstore_tr_t5, platform_app_store_duopoly, theater_ratio, 5, 0.52).
narrative_ontology:measurement(appstore_tr_t10, platform_app_store_duopoly, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(appstore_be_t0, platform_app_store_duopoly, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(appstore_be_t5, platform_app_store_duopoly, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(appstore_be_t10, platform_app_store_duopoly, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_app_store_duopoly, resource_allocation).
narrative_ontology:affects_constraint(platform_app_store_duopoly, digital_markets_act_compliance).
narrative_ontology:affects_constraint(platform_app_store_duopoly, web_app_friction_tax).
narrative_ontology:affects_constraint(platform_app_store_duopoly, developer_bargaining_power_asymmetry).

% DUAL FORMULATION NOTE:
% The app store mandate can be decomposed into two structurally distinct constraints: (1) Technical distribution infrastructure constraint (ε≈0.15, Mountain) — code signing, trusted distribution, fraud prevention are genuine coordination requirements; (2) Rent extraction through payment system monopoly (ε≈0.55, Snare/Tangled Rope) — the 15-30% commission and exclusive payment mandate exceed competitive costs. This story models the unified system (ε=0.58, Tangled Rope) where both operate simultaneously. Downstream constraints (DMA compliance, web app friction) are affected by how this constraint is regulated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_app_store_duopoly, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
