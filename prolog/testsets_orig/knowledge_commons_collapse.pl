% ============================================================================
% CONSTRAINT STORY: knowledge_commons_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_commons_collapse, []).

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
 *   constraint_id: knowledge_commons_collapse
 *   human_readable: Knowledge Commons Collapse
 *   domain: institutional/epistemological
 *
 * SUMMARY:
 *   The knowledge commons collapse refers to the structural transformation of
 *   scholarly knowledge from a collectively maintained and accessible commons
 *   to an enclosed, rent-capturing system dominated by proprietary publishing
 *   institutions and intellectual property regimes. This constraint exhibits
 *   the full spectrum of DR classification depending on the observer's
 *   structural position. The same phenomenon—the digitization of knowledge
 *   combined with artificial scarcity through paywalls, licensing
 *   restrictions, and copyright enforcement—appears as a natural law of
 *   economics (mountain), a coordination mechanism for quality control
 *   (rope), mixed coordination-extraction (tangled rope), pure extraction
 *   (snare), a temporary institutional artifact with open-science sunset
 *   (scaffold), or a degraded regime maintained through performative
 *   legitimation (piton). The extractiveness value (0.68) reflects that the
 *   constraint captures substantial economic value from knowledge seekers
 *   while providing modest coordination benefits. The trajectory shows
 *   acceleration: from 0.35 at the onset of digital publishing, through 0.52
 *   as paywalls hardened, to 0.68 as comprehensive knowledge enclosure
 *   matured. Theater ratio increase (0.42 → 0.65) indicates rising
 *   performative content: increasingly elaborate justifications for paywalls
 *   (peer review, quality control, curation) coexist with empirical evidence
 *   that alternative systems (arXiv, institutional repositories, open-access
 *   journals) provide superior or equal outcomes on all functional metrics.
 *   The constraint manifests through multiple reinforcing mechanisms: paywall
 *   gating, copyright extension, patent pooling, journal bundling, prestige
 *   concentration in high-paywall venues, and career incentives aligned with
 *   proprietary publication.
 *
 * KEY AGENTS:
 *   - Emerging Researchers: Primary victims (powerless/trapped) — dependent on knowledge access for professional development but excluded by cost barriers; cannot exit without abandoning research careers.
 *   - Knowledge Seeking Agents (Global South): Primary victims (powerless/trapped) — bear maximum extraction cost; lack institutional infrastructure and purchasing power to access global knowledge; structural dependence on Northern knowledge institutions.
 *   - Epistemic Commons: Structural victim (powerless/trapped) — abstract collective good that cannot organize or exit; degradation of shared knowledge pool reduces all agents' capacity for cumulative knowledge building.
 *   - Publishing Institutions: Primary beneficiaries (institutional/arbitrage) — capture economic rents through paywall enforcement; maintain arbitrage option to shift business models; experience constraint as coordination mechanism.
 *   - Institutional Researchers: Secondary victims/mixed (moderate/constrained) — bear moderate extraction costs (institutional payment) while gaining prestige benefits; constrained by career incentives aligned with paywall venues; can exit toward open-access publishing but face prestige penalties.
 *   - Open Knowledge Movement: Organized resistance (organized/mobile) — arXiv, PubMed Central, open-access journals, institutional repositories, Wikipedia, open-source communities; building alternative pathways with genuine sunset potential; currently constrained but mobilizing exit capacity.
 *   - Copyright/Patent Regimes: Institutional enforcement mechanism (institutional/arbitrage) — legal apparatus maintaining enclosure; maintains itself through regulatory enforcement; theater-heavy (performative IP rights justification) with increasing gap between legitimation narratives and functional evidence.
 *   - Analytical Observer: Risks naturalizing contingent arrangements as immutable law; scarcity framing treats artificial enclosure as physical constraint; may misidentify extraction mechanism as inevitable coordination cost.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_commons_collapse, 0.68).
domain_priors:suppression_score(knowledge_commons_collapse, 0.72).
domain_priors:theater_ratio(knowledge_commons_collapse, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_commons_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(knowledge_commons_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(knowledge_commons_collapse, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_commons_collapse, snare).
narrative_ontology:human_readable(knowledge_commons_collapse, "Knowledge Commons Collapse").
narrative_ontology:topic_domain(knowledge_commons_collapse, "institutional/epistemological").

domain_priors:requires_active_enforcement(knowledge_commons_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_commons_collapse, rent_capturing_institutions).
narrative_ontology:constraint_victim(knowledge_commons_collapse, knowledge_seeking_agents).
narrative_ontology:constraint_victim(knowledge_commons_collapse, epistemic_commons).
narrative_ontology:constraint_victim(knowledge_commons_collapse, emerging_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMERGING RESEARCHER (SNARE) — Trapped within paywall ecosystems, cannot access foundational literature without institutional affiliation or prohibitive personal costs. Bears full extraction cost while building knowledge. No viable exit — necessity of accessing published research creates structural dependence. Maximum suppression: institutional gatekeeping prevents alternative knowledge pathways.
constraint_indexing:constraint_classification(knowledge_commons_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: KNOWLEDGE SEEKING AGENT IN GLOBAL SOUTH (SNARE) — Trapped by economic barriers and institutional poverty. Cannot access research necessary for local problem-solving. Extraction mechanism: developed-world publishing institutions capture value from knowledge production across the planet while restricting access to that knowledge. Suppression includes infrastructure barriers (internet bandwidth, device access) compounded by paywall gating.
constraint_indexing:constraint_classification(knowledge_commons_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PUBLISHING INSTITUTION (ROPE) — Experiences the constraint as a coordination mechanism. Manages peer review workflow, ensures quality control, aggregates dispersed research into curated collections. These are genuine coordination functions. The institutional actor has arbitrage options: can shift business models, adopt open-access variants, participate in alternative systems. Net beneficiary through extraction leverage.
constraint_indexing:constraint_classification(knowledge_commons_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSTITUTIONAL RESEARCHER (TANGLED ROPE) — Moderately constrained by paywall costs (institution bears them), but also benefits from curation and credential-stamping functions of commercial publishers. High-status publication venues provide career capital. Mixed position: benefits from coordination function while extractive mechanism operates on non-institutional researchers. Can exit through open-access publishing but faces career risks and prestige penalties.
constraint_indexing:constraint_classification(knowledge_commons_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN KNOWLEDGE MOVEMENT (SCAFFOLD) — Organized agents (arXiv, PubMed Central, open-access journals, Wikipedia, institutional repositories) are building alternative pathways that bypass proprietary gates. These alternatives have real sunset potential: as open repositories mature and gain prestige, the paywall extraction mechanism loses force. Currently constrained by network effects and career incentive misalignment, but these barriers are degrading. High suppression currently, but temporally bounded.
constraint_indexing:constraint_classification(knowledge_commons_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COPYRIGHT/PATENT REGIME (PITON) — The legal apparatus maintaining knowledge enclosure persists through institutional inertia despite evidence that alternative regimes (open source, commons-based production) outperform it on innovation and quality metrics. Theater ratio high: extensive performative legitimation (IP rights as 'incentive') despite empirical cases where commons production generates superior outcomes. The regime maintains itself through regulatory enforcement rather than genuine functional necessity.
constraint_indexing:constraint_classification(knowledge_commons_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / SCARCITY NATURALIZATION (MOUNTAIN) — The false mountain view naturalizes information scarcity as physical law. 'Knowledge must be owned because publishing has costs.' 'Paywalls are inherent to quality gatekeeping.' This perspective risks treating contingent institutional arrangements (for-profit publishing, copyright duration, licensing restrictions) as natural laws. The structural data contradicts the mountain classification — the engine will identify this as false summit, revealing naturalization of a contingent economic arrangement.
constraint_indexing:constraint_classification(knowledge_commons_collapse, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_commons_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_commons_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_commons_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_commons_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_commons_collapse, TR),
    TR >= 0.70.

:- end_tests(knowledge_commons_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint captures substantial economic value from knowledge seekers in the form of paywall access fees, institutional subscription costs, and foregone research productivity due to access barriers. The value extraction is substantial because information scarcity is artificial—marginal reproduction cost approaches zero, yet prices remain high. This artificial scarcity is the extraction mechanism. The extractiveness is not maximal (0.95) because some genuine coordination functions exist (peer review does filter, curation does aggregate), and because open-access alternatives demonstrate that these functions can be provided at lower cost. Suppression (0.72): High. Multiple reinforcing mechanisms prevent exit: paywall gating (immediate barrier), copyright/patent law (legal barrier), prestige concentration (career incentive barrier), institutional dependence (infrastructure barrier), and Global South economic poverty (structural barrier). Emerging researchers cannot ignore paywall-published literature without forgoing essential knowledge; Global South agents cannot afford subscription access. The suppression is not total (0.95) because open-access alternatives exist and are growing, though they still face network effect disadvantages. Theater ratio (0.65): Moderate-high and rising. Legitimate coordination functions exist (peer review, quality control, curation) but are increasingly performative. The performative content includes: (1) peer review quality rhetoric that doesn't correlate with actual quality differences between paywall and open-access venues, (2) curation narratives that ignore institutional repository functionality, (3) IP rights justification rhetoric despite commons-based production evidence contradicting it. The theater has increased from 0.42 to 0.65 over the interval as legitimation narratives have become more elaborate while functional evidence diverges from claims.
 *
 * PERSPECTIVAL GAP:
 *   Massive perspectival divergence emerges from this constraint. Publishing institutions genuinely experience it as coordination (rope) — they solve real problems (managing peer review, aggregating research, establishing credibility signals). Institutional researchers see mixed picture (tangled rope) — they benefit from prestige and career capital while bearing paywall costs borne by their institutions. Emerging researchers see pure extraction (snare) — they cannot exit and bear maximum suppression. The open knowledge movement sees temporal containment (scaffold) — they perceive open-access alternatives as approaching critical mass with genuine sunset potential. The copyright regime sees itself as harmless (piton) — reviewers of the IP legal apparatus often describe it as necessary, standard, inevitable, performing essential functions—yet empirical evidence from commons-based production contradicts this. The analytical observer risks seeing natural economic law (mountain) — treating artificial information scarcity as physical constraint, forgetting that knowledge differs fundamentally from scarce resources. The perspectival gap reveals that the constraint's classification depends entirely on structural position: no single type captures the reality for all agents.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position: their power level, exit options, and relationship to extraction flow. Emerging researchers trapped without exit options experience d ≈ 0.95 (maximum target), deriving f(d) ≈ 1.42 from the sigmoid, producing maximum experienced extraction chi. Publishing institutions with arbitrage options experience d ≈ 0.05-0.15 (beneficiary), deriving f(d) ≈ -0.12 to 0.02, experiencing negative or minimal chi—the constraint flows toward them, not away. Institutional researchers with constrained exit (prestige penalties for open-access, career incentives favoring paywall venues) experience d ≈ 0.55-0.65, deriving f(d) ≈ 0.75-0.95, moderate chi reflecting mixed position. Open knowledge coalition with mobile exit (can adopt open alternatives) experience d ≈ 0.40-0.50, deriving f(d) ≈ 0.40-0.65, lower chi reflecting exit capacity and organizational power.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: Knowledge Commons Collapse resolves the mandatrophy by demonstrating that all six types are legitimate perspectival readings of the same structural phenomenon. The mandatrophy question—'Is this coordination or extraction?'—finds that both answers are correct from different positions. Publishing institutions coordinate genuinely AND extract asymmetrically. The coordination and extraction are not competing interpretations but simultaneous structural features. The snare classification for powerless agents is correct: they experience pure extraction with no coordination benefit. The rope classification for publishers is correct: they experience coordination. These are not contradictory; they are perspectival. The scaffold classification is correct: open alternatives are approaching critical mass. The piton classification is correct: performative theater is increasing. The mountain classification is revealed as FALSE SUMMIT: the naturalizing move (treating artificial enclosure as inevitable economic law) misses the contingency of the institutional arrangement. Mandatrophy resolves not through choosing one type but through recognizing that the presheaf of perspectives across the observation site IS the answer. The constraint is tangled rope at the analytical level because it combines genuine coordination (peer review, curation, aggregation) with asymmetric extraction (artificial scarcity, rent capture, suppression of alternatives). The trap of single-position analysis is that beneficiaries see only coordination; victims see only extraction; both miss the hybrid nature visible from the analytical position. The scaffold sunset is real: open-access alternatives demonstrate that coordination functions can be provided at lower extraction cost, which means the current extraction premium (the difference between current paywall pricing and open-access provisioning cost) is pure rent. This rent is declining as open alternatives gain prestige and adoption, but it persists through network effects and career incentive misalignment. The resolution is not inevitable—knowledge commons could collapse completely (snare becomes irreversible) if open alternatives fail to reach critical mass—but the current trajectory shows sunset potential if open-access adoption accelerates.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    marginal_cost_of_reproduction,
    'Does the claimed coordination function (peer review, curation, quality control) justify extraction in an era where marginal cost of knowledge reproduction approaches zero?',
    'Comparative analysis of peer review quality, curation effectiveness, and error detection rates across open-access, institutional repository, and proprietary publishing models. Cost-per-validation accounting.',
    'If coordination value exceeds marginal reproduction cost: constraint may be Tangled Rope (justified extraction). If marginal cost far exceeds coordination value: constraint is Snare (extraction disguised as coordination).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginal_cost_of_reproduction, empirical, 'Whether coordination value justifies extraction given zero marginal reproduction cost').

omega_variable(
    open_science_tipping_point,
    'At what institutional adoption threshold do open-access alternatives reach critical mass and break the paywall extraction mechanism?',
    'Network analysis of citation patterns, institutional repository usage growth, prestige migration toward open venues, corresponding decline in commercial publisher subscription revenue and exclusivity value.',
    'If tipping point < 30% adoption: scaffold sunset is imminent (5-10 years). If tipping point > 60% adoption: paywall constraint may persist as semi-permanent rent extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_science_tipping_point, empirical, 'Adoption threshold for open-access alternatives breaking paywall mechanism').

omega_variable(
    developing_world_knowledge_production,
    'Is the knowledge commons collapse predominantly extracting from knowledge seekers in the Global South, or does it also suppress knowledge production in the Global South?',
    'Analysis of research output origin and circulation patterns; institutional capacity for research in low-income countries; migration of researchers to high-paywall regions; comparison of local problem-solving capacity with and without access to global knowledge commons.',
    'If primarily extraction from seekers: snare classification confirmed. If suppression of production: constraint may be worse than snare—extracting and preventing counter-production simultaneously, approaching totalizing mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developing_world_knowledge_production, empirical, 'Whether Global South experiences primarily extraction or also production suppression').

omega_variable(
    commons_based_production_viability,
    'Can commons-based knowledge production (Wikipedia, open-source software, arXiv preprints, institutional repositories) scale to replace proprietary knowledge gatekeeping without reproducing the same extraction dynamics?',
    'Longitudinal analysis of commons-based projects; identification of cases where commons production became capture/extraction point; models of resource allocation, reputation systems, and governance in successful commons vs failed transitions.',
    'If viable at scale: scaffold sunset realistic, paywall extraction can genuinely dissolve. If commons-based alternatives reproduce extraction patterns: constraint may cycle rather than resolve (snare → open commons → new extraction mechanism → snare).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commons_based_production_viability, empirical, 'Viability of commons-based knowledge production at global scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_commons_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kcc_tr_t0, knowledge_commons_collapse, theater_ratio, 0, 0.42).
narrative_ontology:measurement(kcc_tr_t10, knowledge_commons_collapse, theater_ratio, 10, 0.55).
narrative_ontology:measurement(kcc_tr_t20, knowledge_commons_collapse, theater_ratio, 20, 0.65).
narrative_ontology:measurement(kcc_tr_t5, knowledge_commons_collapse, theater_ratio, 5, 0.48).
narrative_ontology:measurement(kcc_tr_t15, knowledge_commons_collapse, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(kcc_be_t0, knowledge_commons_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(kcc_be_t10, knowledge_commons_collapse, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(kcc_be_t20, knowledge_commons_collapse, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(kcc_be_t5, knowledge_commons_collapse, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(kcc_be_t15, knowledge_commons_collapse, base_extractiveness, 15, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_commons_collapse, information_standard).
narrative_ontology:boltzmann_floor_override(knowledge_commons_collapse, 0.12).
narrative_ontology:affects_constraint(knowledge_commons_collapse, peer_review_theater).
narrative_ontology:affects_constraint(knowledge_commons_collapse, research_funding_concentration).
narrative_ontology:affects_constraint(knowledge_commons_collapse, global_university_stratification).

% DUAL FORMULATION NOTE:
% Knowledge commons collapse decomposes into multiple structurally distinct constraints: (1) paywall_gating (ε≈0.65, snare for individual access barriers), (2) copyright_regime (ε≈0.58, piton for legal enforcement theater), (3) prestige_concentration (ε≈0.72, tangled rope for career incentive coupling), (4) open_access_alternatives (ε≈0.25, scaffold for sunset potential). The aggregate constraint family exhibits the mandatrophy spectrum. Each decomposition story links through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_commons_collapse, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
