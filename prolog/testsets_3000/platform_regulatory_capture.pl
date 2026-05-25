% ============================================================================
% CONSTRAINT STORY: platform_regulatory_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_regulatory_capture, []).

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
 *   constraint_id: platform_regulatory_capture
 *   human_readable: Platform Regulatory Capture
 *   domain: political_economy/technology_regulation
 *
 * SUMMARY:
 *   Platform regulatory capture describes the structural dynamic in which
 *   dominant digital platforms shape the regulatory frameworks nominally
 *   designed to constrain them. This constraint exhibits a tangled mixture of
 *   genuine coordination (regulation does prevent the worst harms: data
 *   breaches, unfiltered algorithmic radicalization, unchecked monopoly
 *   predation) and asymmetric extraction (the platforms themselves define
 *   what counts as 'the worst harms' and ensure that regulation raises
 *   barriers to competitive entry). The constraint's extractiveness has
 *   increased over the interval (0.45 → 0.58) as platforms have matured their
 *   regulatory strategy: from reactive compliance to proactive
 *   standard-setting. Theater ratio has risen correspondingly (0.52 → 0.68),
 *   reflecting that regulatory enforcement produces headlines but minimal
 *   structural change in platform dominance. The constraint operates
 *   simultaneously as a Snare for data subjects (trapped by network effects),
 *   a Rope for platforms (regulation as competitive moat), a Piton for legacy
 *   regulatory frameworks (FTC antitrust doctrine), a Scaffold for emerging
 *   international coalitions (GDPR, DMA), and a false Mountain for
 *   civilizational observers who mistake political contingency for natural
 *   law.
 *
 * KEY AGENTS:
 *   - Dominant Platform Operators (Meta, Google, Amazon, Apple, ByteDance): Primary beneficiary (institutional/arbitrage) — capture regulatory process; benefit from complexity barriers to competition
 *   - Data Subjects and Users: Primary victim (powerless/trapped) — locked into platforms by network effects and cognitive switching costs; subject to surveillance capitalism enabled by regulatory gaps
 *   - Emerging Competitors and Startups: Secondary victim (powerless/trapped or moderate/constrained) — cannot meet compliance costs; lack lobbying power to reshape rules in their favor
 *   - Regulatory Agencies (FTC, SEC, EU DMA enforcers): Captured institution (institutional/constrained with identity_locked dynamics) — dependent on platforms for technical information; staff revolve between agency and industry; identity of 'tech regulator' requires industry legitimacy
 *   - Consumer Privacy Advocates and NGOs: Secondary actor (moderate/constrained) — advocate for stronger regulation but lack resources and expertise to match platform legal teams; benefit nominally from rules but rules are shaped by capture
 *   - International Regulatory Coalition (EU, UK, India): Organized agents (organized/constrained) — building alternative regulatory pathways; creating competitive pressure through jurisdictional divergence
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing capture as inevitable feature of digital markets rather than mutable political outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_regulatory_capture, 0.58).
domain_priors:suppression_score(platform_regulatory_capture, 0.65).
domain_priors:theater_ratio(platform_regulatory_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_regulatory_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_regulatory_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_regulatory_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_regulatory_capture, tangled_rope).
narrative_ontology:human_readable(platform_regulatory_capture, "Platform Regulatory Capture").
narrative_ontology:topic_domain(platform_regulatory_capture, "political_economy/technology_regulation").

domain_priors:requires_active_enforcement(platform_regulatory_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_regulatory_capture, dominant_platform_operators).
narrative_ontology:constraint_victim(platform_regulatory_capture, emerging_competitors).
narrative_ontology:constraint_victim(platform_regulatory_capture, public_interest_constituencies).
narrative_ontology:constraint_victim(platform_regulatory_capture, data_subjects).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DATA SUBJECTS AND EMERGING COMPETITORS (SNARE) — Trapped by network effects and regulatory complexity. Data subjects cannot exit platforms without losing social connectivity; emerging competitors cannot access regulatory space to challenge incumbents. Maximum extraction with minimal coordination function. No viable alternatives; suppression operates through lock-in effects and regulatory capture itself.
constraint_indexing:constraint_classification(platform_regulatory_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CONSUMER ADVOCATES AND PRIVACY REGULATORS (TANGLED ROPE) — Constrained by information asymmetries and resource limitations relative to platform legal teams, but also benefit from coordination that regulation provides (liability frameworks, disclosure standards). Experience mixed coordination and extraction — regulations nominally protect them but are shaped by the regulated entities through lobbying and regulatory capture dynamics.
constraint_indexing:constraint_classification(platform_regulatory_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT PLATFORM OPERATORS (ROPE) — Experience the regulatory constraint as pure coordination. Existing rules, even burdensome ones, entrench competitive advantage by raising barriers to new entrants. High compliance costs are trivial for incumbents but fatal for startups. Regulatory complexity itself becomes a moat. Net beneficiary — regulation coordinates against their competitors.
constraint_indexing:constraint_classification(platform_regulatory_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPTURED REGULATOR (TANGLED ROPE) — Institutional actor whose identity and career incentives are fused with the regulated industry. Genuine coordination function: the regulator prevents the worst harms (major breaches, monopoly predation), but this coordination is asymmetrically extracted: the regulator's effectiveness is limited by regulatory capture (revolving door, information dependence on platforms, identity fusion with industry worldview). Requires active enforcement but enforcement is compromised.
constraint_indexing:constraint_classification(platform_regulatory_capture, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL REGULATORY COALITION (SCAFFOLD) — EU GDPR, Digital Markets Act, and emerging national frameworks (India's data protection bill, UK Online Safety Bill) represent organized agents building alternative regulatory pathways. These are temporary scaffolds with sunset logic: as interoperable standards mature and data portability mandates take effect, the capture mechanism loses leverage. Platforms cannot arbitrage divergent regulatory regimes indefinitely — convergence toward binding standards reduces extraction opportunity.
constraint_indexing:constraint_classification(platform_regulatory_capture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EXISTING REGULATORY FRAMEWORK (PITON) — Antitrust law and consumer protection frameworks designed for pre-internet markets are largely performative: the FTC lacks technical expertise, enforcement authority, and resources to challenge platform dominance meaningfully. Litigation (Meta/Instagram merger review, TikTok bans) are theatrical — the underlying market structure persists. The framework persists through institutional inertia and legitimacy theater, not because it functions effectively. Theater ratio high because enforcement produces headlines but minimal market change.
constraint_indexing:constraint_classification(platform_regulatory_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN — FALSE SUMMIT) — From a civilizational perspective, network effects and information asymmetries between platforms and regulators appear as unchangeable natural laws of digital markets. Platform dominance looks inevitable, regulatory capture looks like a law of nature ('regulatory agencies always get captured'). However, this naturalizes contingent institutional arrangements: regulatory capture is not inevitable. It persists because current political economy makes it rational for regulators to be captured. This perspective risks the oracle gap — the analyst's native position cannot see that the constraint is changeable by restructuring political incentives.
constraint_indexing:constraint_classification(platform_regulatory_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_regulatory_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_regulatory_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_regulatory_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_regulatory_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_regulatory_capture, TR),
    TR >= 0.70.

:- end_tests(platform_regulatory_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms extract significant value through regulatory arbitrage (complying with the minimum necessary in each jurisdiction while maintaining core surveillance/monopoly practices), through capacity to shape rule design (revolving-door access to regulators), and through enforcement gaps (regulations exist but are under-resourced and technically complex). The extraction is not total (Snare-level 0.72) because some regulation does constrain harms and because organized coalitions (EU, international standards) are gradually reducing arbitrage space. Suppression (0.65): High. Data subjects and emerging competitors face substantial barriers: information asymmetries (platforms know their own data flows; regulators don't), network effects (leaving a platform is costly), regulatory complexity (compliance requires expensive legal/technical teams that only giants can afford), and capture itself (the rules are written to entrench incumbents). Suppression is primarily structural for small actors and internalized for data subjects (who have accepted surveillance as the price of connectivity). Theater ratio (0.68): High and rising. Regulatory enforcement is substantially performative: FTC consent decrees rarely change core business models; antitrust litigation spans decades and usually settles with nominal penalties; DMA enforcement is nascent and has not yet produced structural change. The rise in theater (0.52 → 0.68) reflects increasing choreography: platforms are getting better at producing the appearance of compliance while maintaining extraction.
 *
 * PERSPECTIVAL GAP:
 *   The tangled rope classification holds at multiple institutional positions but with different reasons: platforms see rope (coordination), captured regulators see tangled rope (mixed but constrained), independent regulators (where they exist — rare) would see snare (extraction they're trying to stop but can't). The scaffold perspective introduces the critical mutability marker: international coalitions are building structural alternatives that make the capture mechanism less effective. This is the engine's signal that the constraint is not inevitably self-reproducing — it's conditional on the absence of external coordination. The piton and mountain perspectives are risk factors: if enforcement becomes purely theatrical and observers naturalize capture as inevitable, the constraint hardens into a more stable form. The framework's job is to flag that this is a political choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d is computed from agent power, exit options, and beneficiary/victim status. Dominant platforms (institutional/arbitrage) get low d: they are beneficiaries with exit options, so f(d) is negative, meaning the constraint subsidizes them. Data subjects (powerless/trapped) get high d: they are victims with no exit, so f(d) is high (~1.42), meaning they experience maximum extraction. Emerging competitors (powerless/constrained to moderate/trapped) get moderate-to-high d depending on whether they have any alternative market or must compete head-to-head. Regulators in the captured position (institutional/constrained) have an intermediate d with a significant upward override: structural analysis would place them as partial beneficiaries (they gain regulatory authority, career advancement, industry access) but they are also victims (their agency's mission is compromised, their independence is eroded). The override reflects that institutional capture involves identity fusion — the regulator has internalized the industry's framing as their own. This is why identity_locked applies: the captured regulator is not trapped by external barriers (they have legal authority) but by identity fusion (they have become the industry regulator rather than the public interest regulator).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves mandatrophy (the paradox of calling extraction 'coordination') by distinguishing levels. At the platforms' level, regulation IS coordination — it prevents certain harms and provides certainty. At the data subjects' level, the same regulation enables extraction — it creates plausible deniability for harms and regulatory complexity prevents exit. At the international coalition level, the regulation itself becomes a tool for escape — DMA interoperability mandates create structural alternatives. The mandatrophy is real but resolvable: the constraint has a genuine coordination function (preventing catastrophic harms, establishing liability frameworks) AND a genuine extraction function (raising barriers to competition, enabling surveillance capitalism through regulatory approval). It is not pure coordination pretending to extract, nor pure extraction pretending to coordinate. It is both, at different scales, and this is the definition of tangled rope. The classification prevents false categorization in either direction: regulation is neither purely beneficial (rope) nor purely harmful (snare), and pretending one or the other would miss the key insight that the benefits and harms are distributed asymmetrically across power positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_vs_structural_advantage,
    'Is platform dominance driven primarily by regulatory capture or by genuine network effects and scale economies independent of regulation?',
    'Counterfactual analysis: compare platform market shares in jurisdictions with weak vs strong regulation; historical analysis of pre-regulatory dominance patterns; study of platforms that succeeded despite regulatory disadvantage',
    'If capture is primary: deregulation or regulatory restructuring could enable competition (scaffold sunset accelerates). If structural advantage is primary: regulation merely entrenches existing winners (tangled rope persists). Classification sensitivity: high impact on whether capture is mutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_vs_structural_advantage, empirical, 'Whether dominance is driven by regulatory capture or structural advantages').

omega_variable(
    interoperability_implementation_feasibility,
    'Can data portability, interoperability, and open standards genuinely reduce platform network effects, or do technical and social switching costs make exit still prohibitive?',
    'Technical audits of interoperability standards (ActivityPub, DMA obligations); empirical study of user switching rates post-interoperability mandates; cost-benefit analysis of migrations to alternative platforms',
    'If feasible: scaffold sunset is structural (exit becomes mobile rather than trapped), extraction mechanism loses leverage. If infeasible: interoperability mandates are theatrical, capture persists. Classification outcome: determines whether snare is mutable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_implementation_feasibility, empirical, 'Whether interoperability can reduce network effect lock-in').

omega_variable(
    regulatory_capture_reversibility,
    'Are regulatory capture dynamics reversible through institutional reform (independent agency structure, rotational staffing, technical capacity building), or is capture self-reinforcing once established?',
    'Historical analysis of regulatory capture reversals in other industries (telecommunications, energy); study of institutional reforms that increased regulator independence; longitudinal tracking of capture indicators post-reform',
    'If reversible: captured regulator (tangled rope) can be reoriented toward genuine constraint enforcement. If self-reinforcing: capture is nearly permanent, regulator''s identity lock to industry becomes the binding mechanism. Classification implication: determines whether identity_locked should apply to regulator perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_reversibility, conceptual, 'Whether regulatory capture dynamics are reversible').

omega_variable(
    global_coordination_threshold,
    'What fraction of global platforms'' revenue must be subject to binding interoperability/portability requirements before network effects become breakable and competitive entry becomes viable?',
    'Economic modeling of escape velocities from network effects; empirical data on user switching elasticity at varying interoperability coverage; analysis of when alternative platforms (Mastodon, Bluesky) gain traction',
    'Determines credibility of scaffold sunset: if threshold is < 30% coverage, sunset is real and near-term. If threshold is > 70%, scaffold is aspirational. Affects timeline for escape from snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_coordination_threshold, empirical, 'Threshold for interoperability to break network effects').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_regulatory_capture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prc_tr_t0, platform_regulatory_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(prc_tr_t3, platform_regulatory_capture, theater_ratio, 3, 0.59).
narrative_ontology:measurement(prc_tr_t6, platform_regulatory_capture, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(prc_be_t0, platform_regulatory_capture, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(prc_be_t3, platform_regulatory_capture, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(prc_be_t6, platform_regulatory_capture, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_regulatory_capture, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(platform_regulatory_capture, 0.12).
narrative_ontology:affects_constraint(platform_regulatory_capture, network_effects_lock_in).
narrative_ontology:affects_constraint(platform_regulatory_capture, data_surveillance_asymmetry).
narrative_ontology:affects_constraint(platform_regulatory_capture, antitrust_doctrine_obsolescence).
narrative_ontology:affects_constraint(platform_regulatory_capture, international_regulatory_divergence).

% DUAL FORMULATION NOTE:
% Platform regulatory capture is downstream of three structural phenomena: network effects create lock-in conditions; data asymmetries enable surveillance capitalism; antitrust law was designed for industrial-era markets and fails to constrain digital monopolies. This story focuses on the capture mechanism itself. Each upstream constraint has its own ε value reflecting different structural features. The capture constraint links them by explaining how regulation, nominally designed to correct for these problems, becomes captured and entrenches them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_regulatory_capture, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
