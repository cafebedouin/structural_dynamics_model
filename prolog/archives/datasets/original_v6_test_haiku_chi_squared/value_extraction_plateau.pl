% ============================================================================
% CONSTRAINT STORY: value_extraction_plateau
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_value_extraction_plateau, []).

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
 *   constraint_id: value_extraction_plateau
 *   human_readable: The Law of Diminishing Predation
 *   domain: economic/technological
 *
 * SUMMARY:
 *   The Law of Diminishing Predation describes a structural constraint in
 *   dominant platforms and network-based monopolies where extraction from
 *   users, merchants, and content creators has reached biological/economic
 *   saturation. The dominant platform (e.g., Meta, Amazon marketplace,
 *   YouTube) has spent 10-15 years optimizing its capture of surplus value
 *   through algorithmic suppression, fee inflation, terms-of-service changes,
 *   and data monetization. The constraint exhibits a classic snare structure:
 *   users and merchants are trapped by network effects and switching costs,
 *   while the platform experiences extraction as necessary coordination.
 *   Theater has increased as the platform performs 'innovation' and
 *   'community building' while systematically degrading economics for
 *   ecosystem participants. The constraint's distinguishing feature is that
 *   it has hit a plateau: further extraction increases ecosystem collapse
 *   faster than the platform can monetize additional value. User churn
 *   accelerates, merchant margins collapse below viability, creator burnout
 *   reduces content quality, and alternative platforms begin to capture
 *   defectors. The platform perceives the constraint as coordination (keeping
 *   markets efficient), but trapped participants see it as pure extraction.
 *
 * KEY AGENTS:
 *   - Platform Users: Primary victims (powerless/trapped) — locked in by network effects, social data, accumulated profiles, and lack of viable alternatives. Bears extraction through algorithmic suppression, data commodification, attention harvesting.
 *   - Small Merchants: Secondary victims (moderate/constrained) — dependent on platform for customer acquisition but facing margin erosion from fees, algorithmic demotion, and algorithm-driven competition. Can theoretically exit but lose customer base and reputational capital.
 *   - Content Creators: Tertiary victims (powerful/mobile) — despite individual power and audience, locked into platform's monetization scheme and subject to algorithmic demotion, demonetization, and revenue-share cuts. Have capacity to exit but lose accumulated audience.
 *   - Dominant Platform: Primary beneficiary (institutional/arbitrage) — extracts value through advertising, fee collection, data monetization, and merchant/creator surplus capture. Experiences extraction as market-making and platform coordination.
 *   - Regulatory Authority: Secondary stakeholder (organized/constrained) — sees both coordination benefit (efficient digital goods allocation) and extraction problem (consumer harm, economic concentration). Regulation is enforcement that appears to coordinate but actually limits extraction.
 *   - Alternative Platform Ecosystem: Potential constraint disruptor (organized/constrained) — decentralized, federated, or open-source alternatives (Mastodon, Bluesky, Threads, etc.) that reduce lock-in if they achieve critical mass.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing platform dominance as immutable digital law. False summit: what appears inevitable is actually contingent institutional structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(value_extraction_plateau, 0.68).
domain_priors:suppression_score(value_extraction_plateau, 0.72).
domain_priors:theater_ratio(value_extraction_plateau, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(value_extraction_plateau, extractiveness, 0.68).
narrative_ontology:constraint_metric(value_extraction_plateau, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(value_extraction_plateau, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(value_extraction_plateau, snare).
narrative_ontology:human_readable(value_extraction_plateau, "The Law of Diminishing Predation").
narrative_ontology:topic_domain(value_extraction_plateau, "economic/technological").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(value_extraction_plateau, dominant_platform).
narrative_ontology:constraint_victim(value_extraction_plateau, platform_users).
narrative_ontology:constraint_victim(value_extraction_plateau, small_merchants).
narrative_ontology:constraint_victim(value_extraction_plateau, content_creators).
narrative_ontology:constraint_victim(value_extraction_plateau, ecosystem_health).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED USER (SNARE) — Platform dependence for livelihood, social connection, or business visibility. Exit costs are prohibitive: network effects lock in network effects, switching costs are high, alternative platforms lack critical mass. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.64. High extraction, high suppression of alternatives.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MERCHANT (SNARE) — Dependent on platform for customer acquisition and payment processing. Margin erosion from algorithmic demotion, fee extraction, and terms-of-service changes reduces viability. Can theoretically exit but faces customer loss and reputational cost. d≈0.80, f(d)≈1.20, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONTENT CREATOR (SNARE) — Despite high individual power (audience, influence, marketability), locked into platform's monetization scheme. Can exit to competing platform but loses accumulated audience. Algorithmic suppression, demonetization, and revenue-share cuts extract value without direct alternative. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(value_extraction_plateau, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMINANT PLATFORM (ROPE) — Experiences constraint as coordination problem: must maintain user engagement, merchant participation, and ecosystem health to sustain extraction. Extraction is experienced as coordination function — matching users, creators, merchants, and advertisers. d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.06. Net beneficiary; sees own mechanism as market-making.
constraint_indexing:constraint_classification(value_extraction_plateau, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY AUTHORITY (TANGLED ROPE) — Experiences both coordination benefit (efficient allocation of digital goods) and extraction problem (consumer harm, economic concentration). Regulation is enforcement mechanism that appears to coordinate but actually limits platform extraction. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.35.
constraint_indexing:constraint_classification(value_extraction_plateau, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: DIGITAL FEUDALISM NARRATIVE (PITON) — The framing that platform dominance is inevitable, natural, and permanent. This narrative persists despite increasing evidence that extraction has reached saturation: user churn accelerating, merchant margin collapse reducing ecosystem viability, creator burnout reducing content quality. theater_ratio=0.58 reflects that much discourse about platform 'innovation' is actually performative justification for rent extraction. The narrative degrades as extraction becomes unsustainable.
constraint_indexing:constraint_classification(value_extraction_plateau, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN FRAME) — From a civilizational view, network effects and winner-take-most dynamics appear as immutable laws of digital markets. But the structural data (ε=0.68, suppression=0.72, theater=0.58) contradicts a mountain classification. The constraint is contingent on specific institutional arrangements (legal immunity, data privacy gaps, network lock-in via regulation). False summit detected: what appears as digital destiny is actually extractive structure.
constraint_indexing:constraint_classification(value_extraction_plateau, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(value_extraction_plateau_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(value_extraction_plateau, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(value_extraction_plateau, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(value_extraction_plateau, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(value_extraction_plateau, TR),
    TR >= 0.70.

:- end_tests(value_extraction_plateau_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The platform captures surplus through multiple extraction channels: advertising fees (20-30% of merchant revenue), data monetization (user behavioral profiles sold to advertisers), algorithmic suppression (creators cannot reach audience without paid promotion), and terms-of-service control (unilateral fee increases, demonetization policies). The value is concentrated: small number of dominant platforms extract from billions of users, millions of merchants, hundreds of millions of creators. Measurement across decade shows escalation from 0.42 to 0.68 as optimization deepens. Suppression (0.72): High. Alternatives are suppressed through: network effects (utility increases with participant count, creating winner-take-most dynamics), data lock-in (user profiles, social graphs, accumulated reputation non-portable), switching costs (loss of audience, customer base, and accumulated capital), and legal/regulatory barriers (platform immunity rules, data privacy fragmentation that favors incumbents). Suppression increased from 0.60 to 0.72 as platforms invested in moats. Theater ratio (0.58): Moderate-high. Significant performative content in platform narratives: 'innovation' that actually extracts value faster, 'community safety' policies that suppress creators while protecting platform interests, 'creator funds' and 'merchant development' that perform support while degrading actual economics. Theater increased from 0.35 to 0.58 as extraction intensity required more justification. Claimed type (Snare): The base properties meet snare thresholds — ε ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66 from key victim perspectives.
 *
 * PERSPECTIVAL GAP:
 *   The most significant perspectival gap is between the dominant platform's perception (Rope/coordination) and the trapped user's perception (Snare/extraction). The platform legitimately coordinates supply and demand — it matches users with merchants, creators with audiences, and advertisers with targets. This matching function is real and valuable. But the structural data reveals that the platform captures an asymmetric share of the value created by this matching. From the platform's perspective (institutional/arbitrage), χ is negative: the platform is subsidizing the market-making function. From the trapped user's perspective (powerless/trapped), χ is high: they bear the extraction cost. The regulatory authority sees Tangled Rope — both coordination benefit and extraction problem. The analytical observer risks seeing a Mountain (digital destiny, network effects as natural law), but the structural data contradicts this: the constraint is contingent on legal immunity (Section 230 in US), data privacy fragmentation, network lock-in via regulation (interoperability rules missing), and specific business model choices (advertising-based revenue). The piton perspective (digital feudalism narrative) reflects that much discourse about platform inevitability is performative — justifying extraction as natural rather than structural.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform users: Victims + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. Cannot exit without severe cost. Small merchants: Victims + constrained → d≈0.80, f(d)≈1.20. Significant extraction but some exit option (migrate to direct sales, own website, but lose customers). Content creators: Despite high power (audience, marketability), victims + mobile exit option → d≈0.65, f(d)≈1.00. Moderate-to-high extraction because exit is theoretically available but audience loss is severe cost. Dominant platform: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary; derives d from arbitrage exit option and beneficiary status. Regulatory authority: Mixed beneficiary (coordination) and victim (extraction overshooting) + constrained → d≈0.50, f(d)≈0.65. Tangled rope arises from split incentive. Digital feudalism narrative: Institutional + arbitrage → d≈0.08, f(d)≈-0.08. Piton comes from theater gate (theater ≥ 0.70 threshold), not from chi. Analytical observer: analytical → d≈0.72, f(d)≈1.15. Mountain would require ε ≤ 0.25, emerges_naturally=true, accessibility_collapse ≥ 0.85. The structural data fails these gates: ε=0.68 and emerges_naturally=false contradicts mountain.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the platform's Rope perspective is valid but perspectival. The platform DOES coordinate supply and demand; the matching function is real and valuable. But the extraction of asymmetric value from this coordination transforms it into a Snare from the victim's perspective. The mandatrophy appears when the analytical observer or regulator asks: 'Is this coordination or extraction?' The answer is both — it is a Tangled Rope (coordination + enforcement) from the regulatory view, a Rope from the platform's view, and a Snare from the trapped user's view. The critical resolution is that the extraction has hit saturation: further increase in χ will degrade the coordination function faster than it extracts value. At this plateau, the constraint becomes self-defeating — the platform must choose between deeper extraction (risking ecosystem collapse and loss of coordination function entirely) or moderate extraction (accepting lower revenue but maintaining viability). The piton perspective (digital feudalism narrative) masks this choice by naturalizing extraction as inevitable. Regulation that enforces interoperability, data portability, and legal liability could shift the constraint from Snare to Scaffold (temporary coordination problem with sunset) or even to competitive Rope (coordination without asymmetric extraction). The mandatrophy analysis reveals that the classification is stable only if extraction remains at plateau level; either deeper extraction or regulatory intervention will force classification change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraction_saturation_point,
    'At what margin/fee/suppression level does platform extraction become self-defeating, causing ecosystem collapse faster than alternative platforms can capture users?',
    'Longitudinal analysis of churn rates, merchant defection, content creator retention, and cross-platform growth as fee structures intensify. Identify inflection points where ecosystem health metrics turn negative.',
    'If saturation is <15% of potential value: extraction-driven collapse is imminent, snare classification holds but with finite horizon. If saturation is >40%: platform can extract indefinitely, snare is stable/permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_saturation_point, empirical, 'The extraction saturation point at which ecosystem collapse accelerates').

omega_variable(
    alternative_platform_viability,
    'Do decentralized, interoperable, or federated alternatives reduce the lock-in effect sufficiently to make exit mobile rather than trapped for average users?',
    'Network-adoption analysis of alternative platforms (Mastodon, Bluesky, etc.); measurement of switching cost reduction via data portability, identity federation, open standards adoption.',
    'If alternatives gain critical mass: exit options become ''mobile'' and classification shifts from Snare to Tangled Rope for many perspectives. If alternatives remain marginal: exit remains ''trapped'' and Snare persists.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether decentralized alternatives provide viable exit paths').

omega_variable(
    regulatory_intervention_effectiveness,
    'Do antitrust enforcement, interoperability mandates, or data rights regulations actually reduce extraction, or do they create theater while maintaining structural lock-in?',
    'Post-regulation measurement of fee reduction, merchant profitability recovery, creator earnings stability, and user churn. Compare between jurisdictions with vs. without intervention.',
    'If effective: regulation enables exit options (''mobile'' to regulated alternatives), classification shifts toward Scaffold or Tangled Rope. If theater: suppression persists under new guise, Snare classification confirmed with regulatory theater as new suppression mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_effectiveness, empirical, 'Whether regulatory intervention reduces extraction or generates theater').

omega_variable(
    bifurcation_trap,
    'As extraction intensity increases, does the platform bifurcate into separate ecosystems (premium/standard users, exclusive/commodity creators) that actually stabilize the snare by segmenting victims into differently-trapped groups?',
    'Structural analysis of platform segmentation: separate fee tiers, algorithmic ranking by creator status, merchant categories. Measurement of whether segmentation reduces churn or merely redistributes extraction.',
    'If segmentation stabilizes: the snare becomes self-reinforcing through stratification, theater increases, and piton dynamics emerge. If segmentation drives defection: extraction limit is real and structural collapse approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bifurcation_trap, empirical, 'Whether platform bifurcation stabilizes or destabilizes extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(value_extraction_plateau, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vep_tr_t0, value_extraction_plateau, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vep_tr_t5, value_extraction_plateau, theater_ratio, 5, 0.48).
narrative_ontology:measurement(vep_tr_t10, value_extraction_plateau, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(vep_be_t0, value_extraction_plateau, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(vep_be_t5, value_extraction_plateau, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(vep_be_t10, value_extraction_plateau, base_extractiveness, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(value_extraction_plateau, resource_allocation).
narrative_ontology:affects_constraint(value_extraction_plateau, network_effects_winner_take_most).
narrative_ontology:affects_constraint(value_extraction_plateau, data_lock_in_switching_costs).
narrative_ontology:affects_constraint(value_extraction_plateau, algorithmic_transparency_gap).

% DUAL FORMULATION NOTE:
% The Law of Diminishing Predation is downstream of network effects (which create the structural lock-in) and data lock-in (which amplifies switching costs). It is upstream of specific policy interventions (antitrust, interoperability mandates, data rights). The constraint family decomposes as: network_effects (ε≈0.05, Mountain) → value_extraction_plateau (ε≈0.68, Snare) → regulatory_intervention (ε≈0.40, Scaffold). Each story has distinct ε because they measure different structural claims: network effects as natural law, extraction as institutional choice, regulation as temporary enforcement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(value_extraction_plateau, powerful, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
