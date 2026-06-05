% ============================================================================
% CONSTRAINT STORY: platform_app_store_duopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The mobile app store duopoly enforced by Apple iOS and Google Android
 *   represents a hybrid constraint combining genuine coordination functions
 *   (payment processing, malware screening, app discovery) with significant
 *   rent extraction (15-30% commissions on all digital sales). The constraint
 *   has evolved from a legitimate innovation (mid-2000s: app stores
 *   introduced secure, curated distribution) toward increasing extractiveness
 *   as alternative distribution technologies matured but remained restricted
 *   by platform policy. Developers cannot reach meaningful market share
 *   without using the proprietary stores; users cannot access apps without
 *   paying the embedded commission; payment processors and alternative
 *   platforms cannot compete because platform owners control device-level
 *   distribution. The constraint exhibits all six classification types from
 *   different structural positions: indie developers see a pure extraction
 *   snare (d≈0.92); large platforms see a coordination mechanism (d≈0.08);
 *   enterprise developers see mixed benefits and costs (d≈0.32); regulatory
 *   bodies see a partially degraded coordination system requiring
 *   intervention (d≈0.55); end users see hidden price extraction (d≈0.88);
 *   alternative ecosystem builders see a temporary problem solvable by open
 *   standards (d≈0.40). The theater ratio has increased from 0.35 to 0.58 as
 *   curation justifications have weakened relative to actual cost, indicating
 *   gradual shift from functional coordination toward performative control.
 *   The extractiveness has risen from 0.30 to 0.52 as commission rates
 *   stabilized at monopoly levels while developer volume grew, concentrating
 *   profits.
 *
 * KEY AGENTS:
 *   - Apple Inc.: Primary beneficiary (institutional/arbitrage) — captures 15-30% commission on all digital sales; controls distribution gatekeeper role
 *   - Google LLC: Primary beneficiary (institutional/arbitrage) — captures 15-30% commission on Play Store; maintains ecosystem control
 *   - Indie Developers: Primary victim (powerless/trapped) — forced to use app stores; cannot distribute without 99% revenue loss; pay full commission
 *   - Mid-Tier Publishers: Secondary victim (moderate/constrained) — dependent on app stores for 70%+ revenue; limited negotiating power
 *   - End Users: Victim (powerless/trapped) — extraction hidden in app prices; no choice to avoid commission cost
 *   - Enterprise/Platform-Native Developers: Mixed (powerful/arbitrage) — can negotiate exceptions and use alternatives; benefit from ecosystem but pay commission
 *   - Regulatory Coalition: Organized mediator (organized/constrained) — EU, US, South Korea, developer advocacy groups pressing for caps and alternatives
 *   - Alternative Ecosystem Builders: Future competitors (organized/mobile) — PWA, F-Droid, payment processors building parallel channels
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_app_store_duopoly, 0.52).
domain_priors:suppression_score(platform_app_store_duopoly, 0.68).
domain_priors:theater_ratio(platform_app_store_duopoly, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_app_store_duopoly, extractiveness, 0.52).
narrative_ontology:constraint_metric(platform_app_store_duopoly, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_app_store_duopoly, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_app_store_duopoly, tangled_rope).
narrative_ontology:human_readable(platform_app_store_duopoly, "Platform Mandate for Proprietary App Stores and In-App Payments").
narrative_ontology:topic_domain(platform_app_store_duopoly, "technological/platform_economics").

domain_priors:requires_active_enforcement(platform_app_store_duopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, apple_inc).
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, google_llc).
narrative_ontology:constraint_beneficiary(platform_app_store_duopoly, platform_ecosystem_developers).
narrative_ontology:constraint_victim(platform_app_store_duopoly, app_developers).
narrative_ontology:constraint_victim(platform_app_store_duopoly, end_users).
narrative_ontology:constraint_victim(platform_app_store_duopoly, alternative_payment_processors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIE DEVELOPER (SNARE) — Cannot distribute apps outside the duopoly stores without losing 99% of market access. Forced to pay 15-30% commission regardless of alternative cost structure. No meaningful exit. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(platform_app_store_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER PUBLISHER (SNARE) — Can negotiate some terms and maintain web presence but still dependent on app stores for 70%+ of revenue. Exit to alternative distribution (progressive web apps, F-Droid, sideloading) is costly and limits reach. d≈0.78, f(d)≈1.08, σ=1.2 → χ≈0.67.
constraint_indexing:constraint_classification(platform_app_store_duopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: ENTERPRISE/PLATFORM-NATIVE (TANGLED ROPE) — Large firms can negotiate exceptions, use alternative distribution channels (Samsung Galaxy Store, enterprise deployment), and benefit from platform ecosystem stability. Experiences coordination value (SDKs, distribution reach, payment infrastructure) alongside extraction (commission fee). d≈0.32, f(d)≈0.28, σ=1.2 → χ≈0.17.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: APPLE INC. (ROPE) — Experiences the app store mandate as pure coordination mechanism: curated marketplace, unified payments, quality assurance, fraud prevention. Commission captures value created by platform control. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; sees coordination benefit.
constraint_indexing:constraint_classification(platform_app_store_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GOOGLE LLC (ROPE) — Android app store (Play Store) enables ecosystem control, ad targeting, and payment processing. Commission is coordination fee for ecosystem. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; sees coordination benefit.
constraint_indexing:constraint_classification(platform_app_store_duopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY COALITION (TANGLED ROPE) — EU Digital Markets Act, US antitrust actions, South Korea legislation, and app developer advocacy groups (Coalition for App Fairness) see the constraint as both a coordination mechanism worth preserving (ecosystem stability, security) AND extractive rent-seeking that should be capped at cost-plus margin. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.47. Symmetry emerging through regulatory pressure.
constraint_indexing:constraint_classification(platform_app_store_duopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: END USER (SNARE) — Extraction hidden in app prices (apps 15-30% more expensive than direct purchase would allow). No choice to pay lower commission; cost passed through as price or feature reduction. d≈0.88, f(d)≈1.32, σ=1.2 → χ≈0.82.
constraint_indexing:constraint_classification(platform_app_store_duopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 8: HISTORICAL WEB STANDARD (PITON) — From a mid-2000s perspective, app stores were a coordination innovation: central trust authority, secure payments, malware screening. This view now appears as theatrical nostalgia — HTML5, progressive web apps, and direct payment processors (Stripe, PayPal) have matured to provide equivalent coordination at lower cost. theater_ratio=0.58 reflects partial degradation: security/curation services still valuable but increasingly performative given alternatives. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.67.
constraint_indexing:constraint_classification(platform_app_store_duopoly, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 9: ALTERNATIVE ECOSYSTEM (SCAFFOLD) — Progressive web apps (PWAs), open app stores (F-Droid, alternative launchers), unified payment standards (W3C Payment Request API), and sideloading infrastructure are building parallel distribution channels with sunset logic. These alternatives are not yet cost-competitive with app stores but are on trajectory to offer equivalent user experience and developer reach by 2028-2032. d≈0.40, f(d)≈0.40, σ=1.2 → χ≈0.24.
constraint_indexing:constraint_classification(platform_app_store_duopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_app_store_duopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_app_store_duopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_app_store_duopoly, TypeOther, context(agent_power(powerful), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high, reflecting the 15-30% commission as a significant portion of developer revenue but not universally catastrophic. The score reflects that (a) some developers can absorb costs through high-volume sales or negotiated terms; (b) platforms provide real value in payment processing and distribution; (c) alternative distribution is emerging but not yet viable for most developers. The trajectory from 0.30→0.52 shows extraction increasing as regulatory pressure mounts but commissions remain unchanged, shifting perception from 'fair ecosystem cost' to 'unjustified rent.' Suppression (0.68): High. Barriers include: device-level distribution control (Apple prevents sideloading, Android restricts alternatives), regulatory capture (platform lobbying against legislation), technical gatekeeping (SDK restrictions), and network effects (developers must ship on major stores to reach users). However, suppression is not maximal (0.90+) because sideloading is technically possible, regulatory momentum is building, and alternatives are emerging. Theater ratio (0.58): Moderate-high. The security/curation justification (malware screening, fraud prevention) is partly real but increasingly performative. Enterprise developers often access alternative payment processors with equivalent fraud prevention at 2-3% cost. F-Droid provides community curation. The 15-30% commission appears to capture monopoly rent rather than coordination cost. Theater has increased from 0.35 as platforms rely more on 'ecosystem value' and 'innovation support' narratives that feel increasingly hollow as alternatives mature.
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces a six-way perspectival split: (1) Indie developers see pure extraction (Snare, d≈0.92), (2) Platforms see coordination (Rope, d≈0.08), (3) Enterprises see mixed benefits (Tangled Rope, d≈0.32), (4) Regulators see partial degradation (Tangled Rope, d≈0.55), (5) Users see hidden extraction (Snare, d≈0.88), (6) Historical web standards see theatrical degradation (Piton, d≈0.72), (7) Alternative ecosystem sees a solvable problem (Scaffold, d≈0.40). This gap arises because the constraint's structure—monopoly control of distribution—generates different extraction rates depending on the agent's outside options. Powerless agents (indie devs, users) experience maximum extraction because they have no alternatives. Institutional agents (platforms) experience coordination because they created the system. Organized agents (regulators, coalitions) experience a hybrid requiring negotiation. The 10-year measurement trajectory shows increasing performance of the 'theatrical degradation' narrative: as alternatives emerged but remained restricted, the platforms' coordination justifications weakened, shifting the perspective from Rope toward Tangled Rope/Piton.
 *
 * DIRECTIONALITY LOGIC:
 *   Indie developers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum directionality toward extraction. Mid-tier publishers: Victim + constrained → d≈0.78, f(d)≈1.08. High extraction, but negotiation possible. Enterprise developers: Both + arbitrage → d≈0.32, f(d)≈0.28. Can exit, can negotiate; low effective extraction. Apple/Google: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; negative chi indicates coordination benefit. Regulators: Victim (to developer extraction) + constrained → d≈0.55, f(d)≈0.75. Moderate extraction; political power enables intervention. End users: Victim + trapped → d≈0.88, f(d)≈1.32. Maximum extraction; cost passed through prices. The directionality distribution explains why the constraint has become politically salient: the bulk of developers (powerless/constrained, d>0.75) experience acute extraction, while the platforms (d≈0.08) experience coordination benefit. This asymmetry creates coalition pressure for regulation.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY (ε=0.52, below 0.70 threshold). The constraint sits in the contested middle zone: clearly not pure coordination (Rope would require ε<0.45, suppression<0.35), but not pure extraction (Snare would require ε>0.66, suppression>0.60). The tangled_rope classification is correct, but the mandatrophy remains live because the classification depends critically on how one bounds the 'coordination value.' If the 15-30% commission includes 12-15% genuine coordination cost (payment processing, fraud prevention, discovery infrastructure, developer support), then the remaining 3-15% is rent, and Tangled Rope holds. If only 2-5% represents genuine coordination cost, then 10-28% is rent, and the constraint should be classified as Snare. The regulatory interventions (EU DMA, US antitrust cases) are attempts to force a resolution by capping commissions at cost-plus margin (~5-8%), which would shift the classification toward Rope (if caps work) or Scaffold (if implemented with sunset as alternatives mature). The theater ratio increase from 0.35→0.58 suggests the mandatrophy will resolve toward Snare as the coordination justification becomes increasingly theatrical — platforms are losing the rhetorical argument that 30% is fair for 'ecosystem value.' The alternative ecosystem perspective (Scaffold) offers a structural resolution path: if PWAs and alternative app stores reach parity cost and user experience by 2028, the constraint transitions from Tangled Rope to Scaffold with a credible sunset, making the extraction temporary and justifiable as transition mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commission_justification_boundary,
    'What portion of the 15-30% commission fee represents actual coordination value (payment processing, fraud prevention, discovery, curation) versus monopoly rent extraction?',
    'Cost accounting: direct payment processor fees (2-3%), content delivery (1%), fraud/security services (1-2%), curation labor (0.5-1%). Residual >10% would indicate rent extraction. Industry benchmarks from non-monopoly payment processors.',
    'If coordination value ≤ 5%: classification shifts toward Snare across more perspectives. If coordination value ≥ 12%: Rope classification strengthened; Tangled Rope weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commission_justification_boundary, empirical, 'What portion of commission fees represents coordination value vs monopoly rent').

omega_variable(
    developer_exit_capacity_growth,
    'Are progressive web app and alternative app store technologies growing fast enough to provide meaningful exit for developers within the next 5 years?',
    'Market share metrics: PWA adoption rates, F-Droid app count, alternative launcher penetration, sideloading rates. Developer survey data on feasibility of multi-platform distribution without app store dependency.',
    'If exit capacity grows >30% by 2030: scaffold perspective becomes structural (real sunset). If stalled <10%: constraint remains snare/tangled rope indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developer_exit_capacity_growth, empirical, 'Whether alternative distribution can provide meaningful exit within 5 years').

omega_variable(
    platform_monopoly_structural_inevitability,
    'Is app store duopoly inevitable given network effects and device control, or is it contingent on regulatory choice and strategic platform decisions?',
    'Comparative institutional analysis: regulatory interventions (EU DMA, US antitrust), technical feasibility of sideloading and alternative app stores, developer coalition political economy, consumer preference data for open vs curated ecosystems.',
    'If inevitable: constraint appears as Mountain from civilizational perspective (natural tech law). If contingent: false summit detected; constraint is institutional/political, not natural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_monopoly_structural_inevitability, conceptual, 'Whether duopoly is structurally inevitable or contingent on regulatory/strategic choices').

omega_variable(
    user_consent_versus_forced_participation,
    'Does the user''s initial choice to adopt iOS or Android constitute informed consent to app store extraction, or is the extraction forced once lock-in occurs?',
    'User survey: awareness of commission fees, alternative distribution options at purchase time, switching costs after device adoption. Legal analysis of disclosure requirements vs actual practice.',
    'If consent: extraction is coordination-adjacent (Rope). If forced: extraction is pure snare. This determines whether ''user chooses platform'' exempts the constraint from regulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(user_consent_versus_forced_participation, empirical, 'Whether users consent to extraction at platform adoption or face forced participation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_app_store_duopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(appstore_tr_t0, platform_app_store_duopoly, theater_ratio, 0, 0.35).
narrative_ontology:measurement(appstore_tr_t5, platform_app_store_duopoly, theater_ratio, 5, 0.48).
narrative_ontology:measurement(appstore_tr_t10, platform_app_store_duopoly, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(appstore_be_t0, platform_app_store_duopoly, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(appstore_be_t5, platform_app_store_duopoly, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(appstore_be_t10, platform_app_store_duopoly, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_app_store_duopoly, resource_allocation).
narrative_ontology:affects_constraint(platform_app_store_duopoly, digital_marketplace_gatekeeping).
narrative_ontology:affects_constraint(platform_app_store_duopoly, payment_processor_duopoly).
narrative_ontology:affects_constraint(platform_app_store_duopoly, developer_artificial_scarcity).

% DUAL FORMULATION NOTE:
% The app store mandate decomposes into three related constraints: (1) device-level distribution control (platform_app_store_duopoly, ε=0.52, Tangled Rope) — whether developers can distribute outside proprietary stores; (2) payment processor exclusivity (payment_processor_duopoly, ε=0.58, Snare) — whether users must use platform payment methods; (3) developer artificial scarcity (developer_artificial_scarcity, ε=0.45, Tangled Rope) — whether platform SDKs and tooling restrict who can develop. These are structurally distinct (different ε values, different escape mechanisms) but institutionally coupled through platform policy. All three benefit from sideloading restrictions and SDK gatekeeping, making them a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_app_store_duopoly, powerful, 0.32).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
