% ============================================================================
% CONSTRAINT STORY: copyright_protection
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_copyright_protection, []).

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
 *   constraint_id: copyright_protection
 *   human_readable: Copyright Protection Framework
 *   domain: economic/legal/technological
 *
 * SUMMARY:
 *   The copyright protection framework operates as a Tangled Rope constraint
 *   that combines legitimate coordination functions (incentivizing creation,
 *   enabling licensing markets) with substantial extraction mechanisms
 *   (monopolistic pricing, access restriction, suppression of derivative
 *   works, enforcement bureaucracy). The constraint exhibits perspectival
 *   heterogeneity: publishing corporations see pure coordination (Rope),
 *   derivative creators see pure extraction (Snare), independent artists see
 *   mixed effects (Tangled Rope), and open-content movements see a temporary
 *   problem with a sunset (Scaffold). The extractiveness has increased
 *   substantially over the measurement interval (0.30 → 0.52) as digital
 *   reproduction capabilities have reduced coordination costs while legal
 *   enforcement intensity has increased — the constraint has shifted from
 *   coordination-heavy to extraction-heavy. Theater ratio has similarly risen
 *   (0.35 → 0.58), indicating that copyright enforcement increasingly
 *   substitutes performative deterrence (cease-and-desist letters, licensing
 *   bureaucracy) for actual prevention of unauthorized use. The constraint's
 *   suppression value (0.65) reflects legal enforcement mechanisms (DMCA,
 *   cease-and-desist, litigation risk), technological barriers (DRM,
 *   geofencing), and economic barriers (licensing costs). Beneficiaries are
 *   primarily institutional actors (publishing industries, media
 *   corporations) and original creators (who receive first-mover advantage
 *   and exclusive licensing revenue). Victims include derivative creators
 *   (remix artists, adaptive educators, researchers in restricted domains),
 *   knowledge seekers in low-income regions, and the abstract knowledge
 *   commons.
 *
 * KEY AGENTS:
 *   - Original Creators: Primary beneficiary (institutional/arbitrage) — receive exclusive rights, licensing revenue, attribution, reputation. Strong exit via licensing strategy diversification.
 *   - Publishing and Media Corporations: Primary beneficiary (institutional/arbitrage) — monopolistic distribution rights, subscription/DRM licensing revenue, rent extraction through enforcement. Maximum arbitrage capacity.
 *   - Derivative Creators: Primary victim (powerless/trapped) — cannot legally adapt, remix, or build upon existing works without permission. Suppressed through legal liability and enforcement. No exit option.
 *   - Knowledge Seekers in Low-Income Regions: Primary victim (powerless/trapped) — textbooks, medical information, software priced beyond reach. Suppressed through copyright enforcement, geofencing, DRM. Constrained exit.
 *   - Independent Artists: Secondary victim (moderate/constrained) — benefit from copyright protection of their own work but face barriers accessing licensed works for sampling, adaptation, derivative creation. Mixed experience.
 *   - Open Content Movements: Organized coalition (organized/constrained) — Creative Commons, open-source software, open-access publishing. Building alternative frameworks with voluntary licensing. Medium agency, clear exit path.
 *   - Copyright Enforcement Bureaucracy: Institutional actor (institutional/arbitrage) — registration systems, licensing authorities, litigation infrastructure. Maintains inertial function despite reduced necessity. Low functional utility in digital age.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(copyright_protection, 0.52).
domain_priors:suppression_score(copyright_protection, 0.65).
domain_priors:theater_ratio(copyright_protection, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(copyright_protection, extractiveness, 0.52).
narrative_ontology:constraint_metric(copyright_protection, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(copyright_protection, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(copyright_protection, tangled_rope).
narrative_ontology:human_readable(copyright_protection, "Copyright Protection Framework").
narrative_ontology:topic_domain(copyright_protection, "economic/legal/technological").

domain_priors:requires_active_enforcement(copyright_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(copyright_protection, original_creators).
narrative_ontology:constraint_beneficiary(copyright_protection, publishing_industries).
narrative_ontology:constraint_victim(copyright_protection, derivative_creators).
narrative_ontology:constraint_victim(copyright_protection, knowledge_commons).
narrative_ontology:constraint_victim(copyright_protection, cultural_accessibility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DERIVATIVE CREATOR (SNARE) — Cannot exit copyright restrictions without legal liability. Faces suppression through enforcement (DMCA takedowns, cease-and-desist letters, litigation costs). Bears extraction through license fees and permission delays. d≈0.92, f(d)≈1.38, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING WORLD KNOWLEDGE SEEKERS (SNARE) — Trapped by copyright enforcement in low-income regions where textbooks, medical information, and software are priced beyond reach. Cannot exit through legal alternatives. Suppression enforced through DRM, legal barriers, and technological geofencing. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.66.
constraint_indexing:constraint_classification(copyright_protection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDEPENDENT ARTISTS (TANGLED ROPE) — Benefit from copyright protection (exclusive rights, licensing revenue), but also constrained by enforcement costs, bureaucratic permission processes, and inability to access licensed works for sampling, adaptation, or remix. d≈0.58, f(d)≈0.68, σ=1.0 → χ≈0.36.
constraint_indexing:constraint_classification(copyright_protection, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLISHING/MEDIA CORPORATIONS (ROPE) — Primary beneficiary. Experience copyright as pure coordination: enforcement enables licensing revenue streams, exclusive distribution rights, and rental models. Exit via arbitrage (shift to subscription, licensing, DRM). d≈0.08, f(d)≈-0.08, σ=1.2 → χ≈-0.05. Negative effective extraction = net beneficiary.
constraint_indexing:constraint_classification(copyright_protection, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN CONTENT MOVEMENTS (SCAFFOLD) — Organized agents (Creative Commons, open-source software, Wikipedia, open-access publishing) building alternative frameworks with sunset logic. These movements see copyright enforcement as a temporary bottleneck being replaced by voluntary attribution and commons-based peer production. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.31. Extraction is low because the coalition has agency and sees an exit path through license alternatives.
constraint_indexing:constraint_classification(copyright_protection, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT ENFORCEMENT BUREAUCRACY (PITON) — Registration offices, licensing authorities, and litigation systems persist through institutional inertia despite low functional necessity in the digital age. Digital fingerprinting and blockchain-based licensing could replace registry and permission bureaucracies, but enforcement infrastructure maintains itself through legal requirement and path dependence. theater_ratio=0.58 reflects performative enforcement (cease-and-desist letters for unmonitored infringements, settlement negotiations that serve neither party efficiently).
constraint_indexing:constraint_classification(copyright_protection, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, some form of exclusive temporal rights over creation is argued as natural law (reward for innovation, incentive alignment with creator effort). However, the structural data (ε=0.52, suppression=0.65) contradicts this — copyright is a contingent legal construct, not a natural phenomenon. The engine will detect this as a false summit, revealing that 'inherent property rights' rhetoric naturalizes what is actually an extractive institutional choice.
constraint_indexing:constraint_classification(copyright_protection, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(copyright_protection_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(copyright_protection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(copyright_protection, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(copyright_protection, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(copyright_protection, TR),
    TR >= 0.70.

:- end_tests(copyright_protection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): The framework exhibits moderate-high extraction. Original creators benefit from exclusive rights (legitimate coordination), but institutional extractors (publishers, DRM vendors, enforcement bureaus) capture substantial rents through monopolistic licensing, restricted access pricing, and enforcement mechanisms. The extraction exceeds what would be necessary to incentivize creation. Empirical evidence: (1) copyright term extensions (life+70 years in US, rising from life+28 in pre-1976 law) bear no relationship to creator incentives (most creative output occurs in first 5-10 years); (2) licensing revenue to publishers grows while creator royalties stagnate; (3) orphan works (copyrighted but unobtainable for licensing) are locked from use despite zero harm to creator incentives. Suppression (0.65): Moderate-high. Legal suppression includes DMCA criminal liability for circumventing DRM, cease-and-desist enforcement with litigation risk (even for fair-use cases), and international enforcement coordination. Technological suppression (DRM, geofencing, fingerprinting) adds friction. But suppression is not total — some jurisdictions have stronger fair-use protections, open-access norms are growing, and enforcement is difficult in decentralized networks. Theater ratio (0.58): Moderate. Copyright enforcement includes substantial performative elements: cease-and-desist letters are often sent to legitimate fair-use actors to chill speech through litigation threat (not actual infringement); licensing bureaucracy requires permission-seeking for uses that cause zero harm; registration systems require redundant filings despite digital timestamps. However, the framework also includes functional enforcement (DRM prevents casual copying, licensing enables revenue models, some infringement is deterred). The trend shows theater increasing faster than function (digital copying capabilities reduce enforcement effectiveness; bureaucracy persists through inertia).
 *
 * PERSPECTIVAL GAP:
 *   Publishing corporations and derivative creators perceive opposite constraint types from identical structural data. Publishers see Rope (exclusive rights enable licensing markets — a pure coordination function). Derivative creators see Snare (cannot legally adapt works, suppressed through enforcement, trapped with no exit). This gap reflects the asymmetric directionality: publishers benefit (d≈0.08 → low χ), derivative creators are victims (d≈0.92 → high χ). The analytical observer's natural law perspective (copyright as inherent incentive to create) is contradicted by the structural data — copyright duration has extended far beyond empirical creator incentive thresholds, and enforcement suppression exceeds what coordination requires. This marks a false summit: the 'natural law' framing naturalizes what is actually an extractive institutional choice. Open content movements perceive a Scaffold with a real sunset: Creative Commons licensing, blockchain-based licensing, and voluntary attribution models are reducing the necessity of legal copyright enforcement. The sunset logic is credible because alternative incentive mechanisms (reputation, community contribution, access to derivative work) demonstrably motivate creation in open-source software and open-access research.
 *
 * DIRECTIONALITY LOGIC:
 *   Original creators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.08. Net beneficiary. They gain exclusive rights and licensing revenue with minimal coordination cost. Publishing corporations: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Strong net beneficiary. They control distribution monopolies and extract rents through licensing. Derivative creators: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. They cannot legally create derivative works and face legal liability. Knowledge seekers in low-income regions: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction. Textbooks and software are legally inaccessible and enforcement prevents workarounds. Independent artists: Victim + constrained → d≈0.58, f(d)≈0.68. Moderate extraction. They benefit from copyright protection of their own work but face access barriers for derivative work. Open content movements: Organized + constrained → d≈0.45, f(d)≈0.50. Low effective extraction; coalition has agency and alternative pathways.
 *
 * MANDATROPHY ANALYSIS:
 *   The copyright constraint resolves the mandatrophy by acknowledging that the framework serves both genuine coordination functions (incentivizing creation) AND extraction mechanisms (monopolistic licensing, suppression of derivative creation). The classification as Tangled Rope (not pure Rope or pure Snare) reflects this hybrid nature. However, the upward drift in extractiveness (0.30 → 0.52) and theater ratio (0.35 → 0.58) over the measurement interval indicates that the balance has shifted toward extraction. This shift results from: (1) copyright term extensions driven by corporate lobbying (serving extractors, not creator incentives); (2) technological lock-in through DRM and geofencing (suppression amplified); (3) decreasing enforcement functionality (digital networks make perfect enforcement impossible, so theater increases as real suppression decreases); (4) growth of open-content alternatives that meet creator incentive needs without extraction (Creative Commons, open-source software). The mandatrophy is resolved by measuring the perspectival gap: from publishers and aggregators, copyright remains Rope. From derivative creators and low-income knowledge seekers, it is Snare. From independent artists, it is genuinely Tangled Rope. From open-content movements, it is Scaffold with a credible sunset (technical licensing alternatives are viable). The false natural law perspective (copyright as 'inherent' incentive structure) is exposed: creator incentives are empirically satisfied at 5-10 years; current protection (70+ years) serves extraction, not coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_protection_duration,
    'What copyright duration maximizes social welfare: incentivizing creation while allowing cultural evolution and knowledge commons access?',
    'Empirical analysis of creator incentive thresholds (publication rates, investment in creation at different term lengths); measurement of innovation follow-on effects (derivative works, new genres) under different copyright terms; economic modeling of access costs vs creation incentives',
    'If optimal term << 70 years (current US standard): copyright is extracting beyond incentive necessity (Snare strengthened). If optimal term >> 14 years (pre-1976 US standard): term extension justified as coordination (Rope strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_protection_duration, empirical, 'Optimal copyright duration for social welfare').

omega_variable(
    technological_measurement_possibility,
    'Can blockchain-based licensing, digital fingerprinting, and smart contracts replace legal copyright enforcement, eliminating bureaucratic transaction costs?',
    'Comparative analysis of implementation costs (technical infrastructure vs legal enforcement); measurement of transaction completion times and dispute resolution rates across blockchain licensing vs traditional licensing; adoption studies of blockchain licensing platforms',
    'If replacement is feasible: copyright becomes a Scaffold with real sunset (technical infrastructure replaces legal framework within 20-30 years). If not feasible: copyright enforcement bureaucracy persists indefinitely (Piton confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_measurement_possibility, empirical, 'Whether technical licensing infrastructure can replace legal copyright enforcement').

omega_variable(
    access_elasticity_of_creativity,
    'Does restricted access to existing works (through copyright enforcement) reduce or increase cultural creativity and knowledge production?',
    'Historical comparison of creative output under different copyright regimes; longitudinal tracking of derivative work creation rates before/after copyright term extensions; measurement of creative divergence in open-access vs restricted-access domains (e.g., folk music vs copyrighted music genres)',
    'If restriction suppresses creativity: copyright enforcement shifts from Rope/Tangled Rope toward Snare (victims increase, coordination function degrades). If restriction enables creation: current classification reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(access_elasticity_of_creativity, empirical, 'Whether copyright restriction increases or decreases derivative creativity').

omega_variable(
    enforcement_capability_ceiling,
    'In a fully digital environment with decentralized file-sharing and anonymous networks, is copyright enforcement physically possible at scale?',
    'Technical analysis of enforcement mechanisms against decentralized networks; empirical measurement of enforcement success rates (fraction of infringements detected/deterred vs total infringing copies circulated); game-theoretic modeling of enforcement cost vs benefit',
    'If enforcement impossible: suppression value collapses (ε and χ drop dramatically), Tangled Rope degrades to Rope. If enforcement remains viable: current suppression metrics validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capability_ceiling, empirical, 'Whether copyright enforcement remains technically feasible in decentralized digital networks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(copyright_protection, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(copyr_tr_t0, copyright_protection, theater_ratio, 0, 0.35).
narrative_ontology:measurement(copyr_tr_t50, copyright_protection, theater_ratio, 50, 0.48).
narrative_ontology:measurement(copyr_tr_t100, copyright_protection, theater_ratio, 100, 0.58).

% Extraction over time
narrative_ontology:measurement(copyr_be_t0, copyright_protection, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(copyr_be_t50, copyright_protection, base_extractiveness, 50, 0.45).
narrative_ontology:measurement(copyr_be_t100, copyright_protection, base_extractiveness, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(copyright_protection, resource_allocation).
narrative_ontology:boltzmann_floor_override(copyright_protection, 0.35).
narrative_ontology:affects_constraint(copyright_protection, knowledge_commons_access).
narrative_ontology:affects_constraint(copyright_protection, cultural_evolution_constraint).
narrative_ontology:affects_constraint(copyright_protection, innovation_follow_on_effects).

% DUAL FORMULATION NOTE:
% Copyright exists as a single constraint with multiple manifestations depending on observer perspective. The framework cannot be decomposed into separate ε values for different observables — the same legal/technical enforcement mechanism produces different effects for different actors (publishers see coordination; derivative creators see extraction). The perspectival heterogeneity is real, not a measurement ambiguity. The framework influences knowledge commons access (downstream) through restriction; it is influenced by innovation follow-on effects (upstream) through licensing complexity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(copyright_protection, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
