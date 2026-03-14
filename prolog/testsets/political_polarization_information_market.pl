% ============================================================================
% CONSTRAINT STORY: political_polarization_information_market
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_political_polarization_information_market, []).

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
 *   constraint_id: political_polarization_information_market
 *   human_readable: Political Polarization Information Market
 *   domain: political_economy/media_epistemic_systems
 *
 * SUMMARY:
 *   The political polarization information market represents a structural
 *   constraint where institutional actors (partisan media,
 *   engagement-optimized platforms) benefit from fragmented information
 *   environments and polarized public discourse, while the cross-partisan
 *   epistemic commons and low-media-literacy voters bear the costs. The
 *   constraint exhibits characteristics of both pure extraction (snare) and
 *   hybrid coordination-extraction (tangled rope), depending on the
 *   observer's position. The same structural phenomenon—the profitability of
 *   partisan content and attention concentration in polarized niches—appears
 *   as natural tribalism to the analytical observer, as pure extraction to
 *   those seeking common ground, as a solved coordination problem to
 *   beneficiaries, as identity-fused entrapment to captured voters, and as a
 *   temporary institutional failure being addressed through decentralized
 *   alternatives to organized interventionists. The theater ratio (0.68)
 *   reflects that much of the institutional apparatus maintaining the
 *   constraint (fact-checking, credentialing, editorial standards, media
 *   literacy programs) operates performatively, unable to overcome the
 *   structural incentive alignment that makes polarized content more
 *   profitable and engaging than consensus-building content.
 *
 * KEY AGENTS:
 *   - Partisan Media Organizations: Primary beneficiary (institutional/arbitrage) — capture sustained audiences and revenue through polarization-aligned content; could exit by repositioning but choose not to
 *   - Engagement-Optimized Platforms: Primary beneficiary (institutional/arbitrage) — maximize dwell time and advertising revenue through polarization-amplifying algorithms; exit options theoretically abundant but unused
 *   - Cross-Partisan Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good that cannot exit or organize; lacks institutional defender; bears cost of fragmentation
 *   - Low-Media-Literacy Voters: Primary victim (powerless/identity_locked) — structurally mobile but identity-fused through partisan frame; cannot perceive constraint as changeable from within identity
 *   - Local Journalists: Secondary actor (moderate/constrained) — constrained by declining local news economics; benefit from polarization-driven traffic but unable to exit toward sustainable model
 *   - Legacy Media Institutions: Institutional actor (institutional/constrained) — gate-keeping authority atrophied; institutional structures persist through inertia (piton)
 *   - Civil Society Interventionists: Organized actors (organized/constrained) — attempt epistemic maintenance through media literacy and fact-checking; face resource suppression and algorithmic disadvantage
 *   - Decentralized Truth-Maintenance Systems: Emerging institutional ecosystem (organized/constrained) — represent scaffold path with sunset: as cryptographic provenance and interoperable publishing mature, the attention monopoly driving polarization breaks
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements as human nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(political_polarization_information_market, 0.58).
domain_priors:suppression_score(political_polarization_information_market, 0.62).
domain_priors:theater_ratio(political_polarization_information_market, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(political_polarization_information_market, extractiveness, 0.58).
narrative_ontology:constraint_metric(political_polarization_information_market, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(political_polarization_information_market, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(political_polarization_information_market, tangled_rope).
narrative_ontology:human_readable(political_polarization_information_market, "Political Polarization Information Market").
narrative_ontology:topic_domain(political_polarization_information_market, "political_economy/media_epistemic_systems").

domain_priors:requires_active_enforcement(political_polarization_information_market).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(political_polarization_information_market, partisan_media_organizations).
narrative_ontology:constraint_beneficiary(political_polarization_information_market, political_campaign_operatives).
narrative_ontology:constraint_beneficiary(political_polarization_information_market, engagement_optimized_platforms).
narrative_ontology:constraint_victim(political_polarization_information_market, cross_partisan_epistemic_commons).
narrative_ontology:constraint_victim(political_polarization_information_market, low_media_literacy_voters).
narrative_ontology:constraint_victim(political_polarization_information_market, institutional_truth_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CROSS-PARTISAN EPISTEMIC COMMONS (SNARE) — Cannot exit the polarization dynamic; bears full cost of fragmented information environment. The shared epistemic space for genuine deliberation has no advocate and no escape route. Maximum extraction through suppression of bridging information and common reference frames.
constraint_indexing:constraint_classification(political_polarization_information_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LOW-MEDIA-LITERACY VOTER (SNARE) — Structurally mobile (could change media diet, seek diverse sources) but identity-locked through partisan identity fusion. Cannot perceive the constraint as changeable because their identity is constituted through the partisan frame. Trapped within their epistemic bubble despite structural mobility.
constraint_indexing:constraint_classification(political_polarization_information_market, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: LOCAL JOURNALIST (TANGLED ROPE) — Constrained by economic pressure and declining local news economics, but also benefits from traffic generated by polarization-adjacent coverage. Experiences both coordination (informing the community) and extraction (pressure to sensationalize or pick sides for survival). Significant agency costs but some counterbalancing benefit through attention and advertising.
constraint_indexing:constraint_classification(political_polarization_information_market, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PARTISAN MEDIA ORGANIZATION (ROPE) — Primary beneficiary. Experiences the constraint as a coordination mechanism: polarization creates stable audience segmentation, predictable revenue streams, and clear editorial positioning. Low extraction cost because they benefit asymmetrically from the dynamic. Exit options are abundant (could reposition as cross-partisan or boutique coverage), making their continued positioning a choice rather than a constraint.
constraint_indexing:constraint_classification(political_polarization_information_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PLATFORM ENGAGEMENT ALGORITHM (ROPE) — Beneficiary through attention and dwell-time maximization. Experiences the constraint as a pure coordination mechanism: polarized content is algorithmically more engaging. Zero extraction cost because the platform's optimization goal and the polarization dynamic are perfectly aligned. The platform's exit options are theoretical but unused — they could deprioritize divisive content but choose not to.
constraint_indexing:constraint_classification(political_polarization_information_market, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY MEDIA INSTITUTIONAL AUTHORITY (PITON) — Traditional broadcast and print institutions that once anchored epistemic consensus. Their authority and gate-keeping function have atrophied as audiences fragment, but the institutional structures (newsrooms, editorial boards, broadcast licenses) persist. Theater ratio (0.68) reflects that institutional authority rituals (fact-checking sections, editorial standards, credentialing) continue performatively despite loss of actual consensus-maintenance function. Maintained through inertia.
constraint_indexing:constraint_classification(political_polarization_information_market, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: CIVIL SOCIETY INTERVENTION COALITION (TANGLED ROPE) — Organized agents (media literacy nonprofits, fact-checkers, digital literacy programs) working to counter polarization experience both coordination (genuine epistemic maintenance) and extraction (resource constraints, capacity limits, asymmetric reach vs. polarization production). Their efforts coordinate information access but face suppression through resource starvation and algorithmic disadvantage vs. engagement-optimized polarized content.
constraint_indexing:constraint_classification(political_polarization_information_market, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: INSTITUTIONAL TRUTH-MAINTENANCE ECOSYSTEM (SCAFFOLD) — Emerging decentralized structures (community notes, distributed fact-checking, interoperable protocols, digital provenance systems) represent a temporary coordination fix with sunset logic. As these mature, the platform-monopoly polarization mechanism loses force — when any publisher can reach any audience through multiple pathways and facts are cryptographically verifiable, the attention monopoly that drives engagement-optimized polarization breaks. Estimated sunset: 15-20 years for decentralized verification and publisher interoperability to mature.
constraint_indexing:constraint_classification(political_polarization_information_market, scaffold,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / FALSE NATURAL LAW VIEW (MOUNTAIN) — The 'polarization is natural' framing claims that humans naturally sort into tribal groups and that information technology amplifies this inherent tribalism. But this naturalizes contingent institutional choices: algorithmic engagement optimization, media consolidation, declining local journalism economics, and advertiser-driven incentives are not laws of nature. The mountain classification is a false summit — the engine's structural data reveals contingent architecture, not natural law.
constraint_indexing:constraint_classification(political_polarization_information_market, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(political_polarization_information_market_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(political_polarization_information_market, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(political_polarization_information_market, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(political_polarization_information_market, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(political_polarization_information_market, TR),
    TR >= 0.70.

:- end_tests(political_polarization_information_market_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from the epistemic commons and low-literacy voters through attention capture, information quality degradation, and suppression of bridging content. But extraction is not at snare maximum because some coordination genuinely occurs (partisan media do inform their audiences, platforms do connect people). The extraction is layered onto a coordination function rather than pure. The trajectory from 0.32 to 0.58 reflects the compound effect of platform engagement optimization and media consolidation over 16 years. Suppression (0.62): High. Multiple barriers prevent exit from polarization: algorithmic amplification disadvantages bridging content, partisan identity fusion makes diversity-seeking costly, declining local news economics reduce independent alternatives, and media literacy investments are chronically underfunded relative to engagement-optimization R&D. Theater ratio (0.68): High. Institutional authority rituals (fact-checking sections, editorial standards, media literacy programs, credentialing systems) persist despite low actual consensus-maintenance function. These theatrics create the impression of epistemic gatekeeping while the actual gatekeeping (algorithmically determined information flow) occurs outside their visibility. Legacy institutions maintain legitimacy through performed authority rather than actual power.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap separates beneficiaries (who see rope/coordination) from victims (who see snare/extraction). Within the victim class, identity-locked voters perceive mountain (polarization is natural) while analytically engaged observers perceive snare or tangled rope (polarization is institutional). The piton perspective on legacy institutions reveals how institutional authority persists through theater despite functional atrophy. The scaffold perspective from decentralized advocates represents a real structural alternative path (cryptographic provenance, interoperable publishing) that could sunset the polarization market's extraction mechanism, but this path requires 15-20 years and faces resistance from current beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary directionality: Partisan media and platforms are beneficiaries with arbitrage-level exit options (they choose polarization optimization). Their d values are low (~0.15-0.25), producing negative effective extraction through the sigmoid—they experience the constraint as beneficial coordination. Victim directionality: The epistemic commons is a powerless trapped victim with d~0.95, experiencing maximum extraction. Low-media-literacy voters face identity_locked exit (structurally mobile but cognitively fused), producing d~0.89, also high extraction. The local journalist faces constrained exit with split victim/beneficiary status (benefits from traffic, bears cost of race-to-sensationalism), producing moderate d~0.65. Civil society interventionists are organized victims with constrained exit, producing d~0.55. Scope modifiers: National scope for media consumption patterns, but global scope for platform infrastructure and engagement optimization. The scope amplifies beneficiary extraction (global platforms extract across all jurisdictions) while constraining victim remediation (epistemic commons cannot be rebuilt at local scale alone).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in political polarization resolves through recognizing that the 'polarization is natural' mountain framing is a false summit. Institutional incentives (engagement optimization, media consolidation, advertiser business models) are doing the causal work; tribalism is the cover story. The true classification is tangled rope (hybrid coordination-extraction) or snare (pure extraction from epistemic commons), not mountain (immutable law). The false natural law claim serves legitimation by suggesting that regulation or institutional change is futile—if tribalism is inherent, intervening in media incentives won't help. Mandatrophy detection: when 'polarization is human nature' appears in policy discourse to resist algorithmic transparency, media literacy funding, or interoperability mandates, the natural law claim is performing ideological work. This reveals the false summit and re-anchors the classification as tangled rope/snare, making institutional intervention analytically coherent rather than futile.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_exit,
    'Is the low-media-literacy voter''s entrapment primarily identity-locked (internalized partisan frame) or primarily constrained (material barriers to accessing diverse information)?',
    'Experimental intervention: provide frictionless access to diverse information through trusted channels while maintaining partisan identity. Track behavior change. If behavior changes substantially, suppression is primarily material (constrained). If behavior persists despite access, suppression is primarily identity-locked (cognitive capture).',
    'If identity_locked: constraint is more durable because it requires identity-frame dissolution, not just information access. Snare classification is robust. If constrained: constraint is more tractable — reducing friction to diverse sources would shift exit_options to mobile and reshape classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_exit, empirical, 'Whether voter polarization is identity-fused or material-barrier suppression').

omega_variable(
    engagement_metric_causality,
    'Does engagement-optimized algorithm design *cause* polarization production, or does it merely *amplify* pre-existing polarization preferences?',
    'Counterfactual analysis: A/B test engagement algorithms with polarization dampening (deprioritize divisive content) against standard optimization. Measure engagement change and content diversity change. If engagement drops significantly, causality is strong. If engagement holds steady, the amplification hypothesis is stronger.',
    'If algorithms cause polarization: rope/tangled_rope classification of platform is justified (they create the coordination problem they solve). If they amplify pre-existing: institutional beneficiary status is less clear (they respond to demand rather than create demand). Affects whether platform exit is arbitrage (choice) or constrained (demand-driven necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(engagement_metric_causality, empirical, 'Whether engagement algorithms cause or amplify polarization').

omega_variable(
    cross_partisan_epistemic_commons_restoration,
    'Can a shared epistemic space for genuine deliberation be restored through institutional intervention (public media, media literacy, fact-checking infrastructure) or is the fragmentation irreversible without technological change?',
    'Observational study of interventions in comparable democracies; measurement of epistemic consensus indicators (alignment on basic facts, common reference frames) before/after intervention. Historical comparison with pre-internet political discourse.',
    'If restorable through institutional means: snare victim status is temporary, and organized interventions could shift to rope. If restoration requires technological change (decentralization, cryptographic provenance): scaffold perspective''s sunset timeline is the only viable exit path.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cross_partisan_epistemic_commons_restoration, empirical, 'Whether epistemic commons can be restored through institutional intervention').

omega_variable(
    partisan_sorting_threshold,
    'Below what threshold of information access inequality does political polarization shift from extractive (snare/tangled_rope) to coordination (rope)?',
    'Comparative analysis across democracies with different media infrastructure, media literacy investment, and algorithmic regulation. Identify threshold where diversity indicators (cross-partisan media consumption, epistemic consensus measures) maintain functional deliberation despite political disagreement.',
    'If threshold is low (even modest diversity suffices): small interventions could shift classification significantly. If threshold is high (require near-equal access): systemic architectural change is necessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partisan_sorting_threshold, empirical, 'Threshold of information access inequality for sustainable deliberation').

omega_variable(
    mandatrophy_false_natural_law,
    'Does the ''polarization is natural'' framing (mountain perspective) succeed in legitimizing what is actually an institutional extraction mechanism?',
    'Discourse analysis of policy resistance: track how ''tribalism is human nature'' framing appears in opposition to regulation, media literacy funding, or algorithmic transparency. If the natural law claim is doing legitimation work, mandatrophy is active.',
    'If framing is doing legitimation work: false summit detection is confirmed, and the mountain classification reveals a conceptual trap. If the framing is merely descriptive: mountain perspective is analytically neutral.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_false_natural_law, conceptual, 'Whether naturalization framing succeeds in legitimizing institutional extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(political_polarization_information_market, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(polpol_tr_t0, political_polarization_information_market, theater_ratio, 0, 0.45).
narrative_ontology:measurement(polpol_tr_t8, political_polarization_information_market, theater_ratio, 8, 0.57).
narrative_ontology:measurement(polpol_tr_t16, political_polarization_information_market, theater_ratio, 16, 0.68).

% Extraction over time
narrative_ontology:measurement(polpol_be_t0, political_polarization_information_market, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(polpol_be_t8, political_polarization_information_market, base_extractiveness, 8, 0.45).
narrative_ontology:measurement(polpol_be_t16, political_polarization_information_market, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(political_polarization_information_market, information_standard).
narrative_ontology:affects_constraint(political_polarization_information_market, algorithmic_engagement_optimization).
narrative_ontology:affects_constraint(political_polarization_information_market, media_consolidation).
narrative_ontology:affects_constraint(political_polarization_information_market, local_news_economic_collapse).
narrative_ontology:affects_constraint(political_polarization_information_market, partisan_identity_fusion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(political_polarization_information_market, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
