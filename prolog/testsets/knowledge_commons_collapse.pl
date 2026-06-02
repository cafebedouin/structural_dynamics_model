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
 *   human_readable: Knowledge Commons Collapse: Scholarly Knowledge Enclosure and Rent Capture
 *   domain: institutional/epistemological
 *
 * SUMMARY:
 *   The knowledge commons collapse represents a structural transformation in
 *   how scholarly knowledge is organized and accessed. Over the past 40
 *   years, the scholarly knowledge system has shifted from a collectively
 *   maintained commons — where institutions and researchers viewed knowledge
 *   access as a public good — to an enclosed, rent-extracting system
 *   dominated by proprietary publishers and intellectual property regimes.
 *   This constraint exhibits all eight DR types from different structural
 *   positions, making it a diagnostic exemplar for how institutional
 *   arrangements appear as natural law to some observers while manifesting as
 *   pure extraction to others. The same phenomenon — the gap between
 *   knowledge creation and knowledge access — appears as an immutable
 *   property of information goods (mountain), a coordination mechanism for
 *   scholarly communication (rope), a mixed coordination-extraction hybrid
 *   with gatekeeping (tangled_rope), pure extraction with no coordination
 *   benefit (snare), a temporary problem being solved through open-access
 *   mandates (scaffold), or a degraded ritual maintained through
 *   institutional inertia (piton), depending on the observer's structural
 *   position in the knowledge economy.
 *
 * KEY AGENTS:
 *   - Global Knowledge Access: Primary victim (powerless/trapped) — the collective good of accessible scholarly knowledge cannot exit or organize; bears full cost of enclosure
 *   - Developing World Scholars: Primary victim (powerless/trapped or identity_locked) — trapped by paywalls, lack of capital, and geographic access barriers. Some experience identity lock (cannot imagine themselves outside academia despite structural mobility to alternative careers)
 *   - Early-Career Researchers: Secondary victim (powerless/identity_locked) — structurally mobile (could leave academia) but identity-fused with academic identity; publish-or-perish metrics enforce dependence on expensive venues and paywalled prestige markers
 *   - Research Institutions and Librarians: Mixed actors (moderate/constrained) — benefit from negotiating access but face rising journal costs; coordinate research access while being squeezed for licensing fees
 *   - Academic Publishers: Primary beneficiary (institutional/arbitrage) — extract rents through proprietary publishing model; perceive the constraint as pure coordination with low-cost infrastructure
 *   - IP Regime Administrators: Secondary beneficiary (institutional/arbitrage) — enforce patent and copyright regimes that protect publisher monopolies; perceive extraction as legitimate incentive structure
 *   - Open Science Movement: Organized agents (organized/constrained) — building alternative commons infrastructure (preprint servers, institutional repositories, open-access mandates); experience both coordination benefits and extraction costs from IP enforcement
 *   - Funder-Mandated Open Access: Institutional actor (institutional/arbitrage) — NIH, EU Horizon, Gates Foundation imposing open-access mandates; creating temporary alternative pathways with planned sunset as norms mature
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent properties of knowledge economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_commons_collapse, 0.58).
domain_priors:suppression_score(knowledge_commons_collapse, 0.68).
domain_priors:theater_ratio(knowledge_commons_collapse, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_commons_collapse, extractiveness, 0.58).
narrative_ontology:constraint_metric(knowledge_commons_collapse, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(knowledge_commons_collapse, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_commons_collapse, tangled_rope).
narrative_ontology:human_readable(knowledge_commons_collapse, "Knowledge Commons Collapse: Scholarly Knowledge Enclosure and Rent Capture").
narrative_ontology:topic_domain(knowledge_commons_collapse, "institutional/epistemological").

domain_priors:requires_active_enforcement(knowledge_commons_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_commons_collapse, academic_publishers).
narrative_ontology:constraint_beneficiary(knowledge_commons_collapse, ip_regime_administrators).
narrative_ontology:constraint_victim(knowledge_commons_collapse, global_knowledge_access).
narrative_ontology:constraint_victim(knowledge_commons_collapse, researcher_autonomy).
narrative_ontology:constraint_victim(knowledge_commons_collapse, developing_world_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPING WORLD SCHOLAR (SNARE) — Trapped by journal paywalls, limited institutional access, and lack of capital to purchase article access or APCs. Suppressed through geographic pricing, language barriers, and IP enforcement. Experiences maximum extraction with zero exit options at biographical timescale. Cannot exit without abandoning scholarship itself.
constraint_indexing:constraint_classification(knowledge_commons_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EARLY-CAREER RESEARCHER (SNARE) — Structurally mobile (can change careers) but identity-fused with academic identity. Trapped by publish-or-perish metrics that require expensive open-access fees or journal prestige (paywalled venues). The binding is cognitive: the researcher cannot imagine themselves outside academia despite structural mobility, making the constraint functionally equivalent to trapped for biographical horizon.
constraint_indexing:constraint_classification(knowledge_commons_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: RESEARCH INSTITUTION LIBRARIAN (TANGLED ROPE) — Constrained by budget cycles and journal licensing contracts but coordinates genuine scholarly access for their institution. Experiences extraction through rising journal costs but also benefits from negotiating favorable subscription rates. Mixed coordination (enabling research) and extraction (being squeezed for licensing fees). Exit is costly (institutional reputation damage, researcher dissatisfaction) but possible.
constraint_indexing:constraint_classification(knowledge_commons_collapse, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMIC PUBLISHER (ROPE) — Experiences the constraint as a coordination mechanism: connecting authors to readers, managing peer review, maintaining research archives. The publisher perceives pure coordination with low-cost platform maintenance and subscription licensing. Full beneficiary with exit options (can shift to open-access or subscription models). Experiences zero or negative effective extraction — the constraint subsidizes this agent.
constraint_indexing:constraint_classification(knowledge_commons_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE MOVEMENT (TANGLED ROPE) — Organized agents (institutional repositories, preprint servers, open-access mandates) experience both coordination benefits (building alternative infrastructure) and extraction costs (competing against entrenched publishers, funding constraints). Constrained by IP enforcement and dominant publisher gatekeeping. Generational horizon reflects that building alternative commons requires sustained institutional commitment over decades. Mixed experience: genuine coordination function with significant extraction overhead from IP regimes.
constraint_indexing:constraint_classification(knowledge_commons_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: FUNDER-MANDATED OPEN ACCESS (SCAFFOLD) — Funders (NIH, EU Horizon, Gates Foundation) impose open-access mandates, creating alternative publication pathways and institutionalizing open licensing. Low effective extraction because mandates have enforcement power and an explicit sunset: as institutional open-access norms mature, traditional publisher gates lose leverage. Temporary coordination mechanism with a planned phase-out (sunset: ~15-20 years as national research systems fully adopt mandates).
constraint_indexing:constraint_classification(knowledge_commons_collapse, scaffold,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: PEER REVIEW THEATER (PITON) — Traditional peer review as gatekeeping mechanism is largely performative: publishers extract legitimacy and authority from the review ritual while the actual quality control is provided by volunteer reviewers and editors. The constraint persists through institutional inertia (tenure committees still value journal prestige) despite degraded function (reviews increasingly superficial due to reviewer load). Theater ratio is high because the review process's performance (legitimacy signaling) outweighs its function (quality assurance).
constraint_indexing:constraint_classification(knowledge_commons_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the tension between knowledge sharing and institutional sustainability appears as an immutable property: knowledge creation requires institutions, institutions require revenue, and information goods create natural monopolies. This perspective risks naturalizing what is actually a contingent choice: IP regimes and publisher gatekeeping are policy decisions, not laws of nature. The engine's false summit detector will identify this as naturalization of a contestable institutional arrangement.
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
    constraint_indexing:constraint_classification(knowledge_commons_collapse, TypeOther, context(agent_power(moderate), _, _, _)),
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
 *   Extractiveness (0.58): High-moderate, reflecting the asymmetry between publishers' benefits and researchers'/institutions' costs, but not maximal because significant open-access alternatives now exist and funder mandates are creating exit pathways. The measurement trajectory shows acceleration (0.28 → 0.58) over 20 years, reflecting increasing publisher consolidation and rising journal costs. Suppression (0.68): High, indicating substantial barriers to knowledge access: paywalls, institutional subscription costs, geographic pricing, language barriers, and IP enforcement. Suppression remains stable (not declining) despite open-access growth because traditional publishers maintain gatekeeping power over prestige venues and researchers face citation incentives favoring paywalled journals. Theater ratio (0.64): Moderate-high, reflecting that peer review has shifted toward performative legitimacy signaling rather than functional quality assurance. The ratio rises over the interval (0.38 → 0.64) as reviewer capacity declines relative to submission volume, making actual substantive review increasingly impossible — the theater persists as a legitimacy ritual while real quality assessment moves to post-publication community scrutiny. Claimed type (tangled_rope): The constraint requires active enforcement (IP law, publisher contracts, license agreements), has clear beneficiaries (publishers, IP administrators), and has clear victims (scholars, institutions, developing-world access). It exhibits coordination benefits for publishers (connecting authors to readers) alongside asymmetric extraction (monopoly pricing, controlled dissemination), making it structurally a hybrid coordination-extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates radical perspectival divergence. Developing-world scholars perceive pure snare (trapped, high extraction, no agency). Early-career researchers perceive snare through identity-lock (structurally mobile but cognitively captured). Research institutions perceive tangled rope (genuine coordination mixed with extraction). Publishers perceive rope (pure coordination mechanism with platform benefits). Open-science advocates perceive tangled rope (building alternatives while fighting IP enforcement). Funders perceive scaffold (temporary problem with sunset solution). The peer-review system perceives itself as rope (quality assurance) but appears as piton (degraded theater) from analytical perspective. The civilizational analytical observer risks seeing mountain (natural information-goods economics) but structural data reveals this as false summit: identical mechanisms operate under different policy regimes with dramatically different extractiveness outcomes. The perspectival gap reflects real structural differences in agent power, exit options, and beneficiary status — not mere disagreement about evaluation.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural position: beneficiaries with arbitrage options (publishers) derive d from their ability to capture rents without exit costs, producing low d → negative f(d) → negative effective extraction (the constraint subsidizes them). Victims with trapped or identity-locked exits (researchers, developing-world scholars) derive d from their inability to exit, producing high d → high f(d) → high experienced extraction. Organized agents (open-science movement) with constrained but coordinated exits derive moderate d → moderate f(d) → moderate extraction. Institutional actors (research libraries, open-access funders) with arbitrage options split: libraries experience higher d due to being squeezed in the middle (moderate-to-high f(d)), while funders experience low d due to their ability to bypass traditional publishers entirely. The scope modifier σ(S) = 1.2 (global scope) amplifies effective extraction, making the global dimension of the constraint particularly severe for resource-constrained agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The knowledge commons collapse resolves the mandatrophy by demonstrating that all eight types are legitimate perspectival readings of the same institutional arrangement. The constraint is not one fixed type; it is a **presheaf of types over observer positions**. The mandatrophy's resolution is structural: the question is not 'which type is correct?' but 'what is the distribution of experienced constraint-types across all agents, and what does that distribution reveal about the system's asymmetries?' The false summit classification at the analytical/civilizational context is critical: it reveals that naturalizing the commons collapse as 'inevitable information economics' conceals policy choices (IP regimes, publisher licensing, prestige metrics) that could be reversed. The constraint exhibits classic mandatrophy structure: (1) Uniformity claim: 'Knowledge enclosure is natural.' (2) Perspectival reality: Different agents experience radically different constraints. (3) Resolution: The uniformity claim is a false summit — it naturalizes contingent institutional arrangements. The open-science movement's scaffold perspective reveals that alternative institutional arrangements are structurally possible; the constraint's immutability is not epistemic, it is political.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_quality_measurement,
    'Does traditional peer review actually improve research quality, or does it primarily signal institutional legitimacy?',
    'Longitudinal tracking of post-publication corrections and retractions for preprint-only papers vs peer-reviewed papers; blind comparison of research quality metrics between peer-reviewed and alternative quality-assurance mechanisms (community review, registered reports, multi-tier open evaluation)',
    'If peer review improves quality: theater_ratio should be lower (functional component is substantial). If purely legitimacy signaling: theater_ratio ≥ 0.80 confirms piton classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_quality_measurement, empirical, 'Whether traditional peer review has functional quality benefit or is purely performative').

omega_variable(
    alternative_commons_viability,
    'Can decentralized open-access infrastructure (arXiv, institutional repositories, community repositories) sustain long-term scholarly knowledge commons without corporate publisher mediation?',
    'Longitudinal tracking of adoption rates, sustainability models, and quality metrics for open-access alternatives; cost analysis of decentralized vs centralized publication infrastructure; governance stability assessment of open-science platforms',
    'If viable: scaffold perspective is correct and the knowledge commons collapse is reversible via institutional commitment (sunset = 15-20 years). If not viable: the collapse is structural and only open-access mandates can create a commons (mandates become permanent, not temporary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_commons_viability, empirical, 'Whether decentralized open-access infrastructure can sustain knowledge commons').

omega_variable(
    ip_regime_necessity,
    'Is intellectual property protection necessary to incentivize knowledge production, or does it reduce access and innovation without increasing production?',
    'Comparative analysis of knowledge production rates and quality in high-IP-protection vs low-IP-protection regimes (comparing pharmaceutical/software IP vs academic publishing where IP is typically assigned to institutions, not creators); empirical studies of innovation incentives without IP protection (Wikipedia, Linux, open-science collaborations)',
    'If IP necessary: the commons collapse is unavoidable given competitive pressure (institutional/extraction perspective justified). If not necessary: the collapse is a policy choice that could be reversed (false summit identification justified).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ip_regime_necessity, empirical, 'Whether IP protection is necessary to incentivize knowledge production').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression of knowledge access primarily structural (paywalls, bandwidth, institutional access barriers) or internalized (researchers and institutions accepting IP regime legitimacy as natural)?',
    'Post-access behavioral tracking: if suppression is structural, institutions with open-access mandates show markedly higher research collaboration and citation across borders. If internalized, open access produces no change in collaboration patterns (researchers still cite prestige venues preferentially, limiting access doesn''t change behavior).',
    'If structural: removing barriers (mandates, open-access) reduces effective suppression. If internalized: effective suppression remains high despite open access (the constraint persists through cognitive capture).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural barrier or internalized acceptance').

omega_variable(
    false_summit_natural_law,
    'Is the knowledge commons collapse an inevitable feature of knowledge economics (natural law) or a contingent institutional arrangement (policy choice)?',
    'Historical comparison: Did knowledge enclosure occur at similar rates and intensities across different national IP regimes, or did policy choices significantly modulate the outcome? Counterfactual analysis of alternative institutional arrangements (e.g., public-utility models for scholarly publication, mandatory open-access from inception rather than after publication delay).',
    'If natural law: mountain classification justified; the constraint is immutable. If contingent: false summit; the constraint is reversible via policy change and institutional commitment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_natural_law, conceptual, 'Whether knowledge commons collapse is natural law or policy choice (FSM candidate)').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_commons_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kcc_tr_t0, knowledge_commons_collapse, theater_ratio, 0, 0.38).
narrative_ontology:measurement(kcc_tr_t10, knowledge_commons_collapse, theater_ratio, 10, 0.52).
narrative_ontology:measurement(kcc_tr_t20, knowledge_commons_collapse, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(kcc_be_t0, knowledge_commons_collapse, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(kcc_be_t10, knowledge_commons_collapse, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(kcc_be_t20, knowledge_commons_collapse, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(kcc_su_t0, knowledge_commons_collapse, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(kcc_su_t10, knowledge_commons_collapse, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(kcc_su_t20, knowledge_commons_collapse, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_commons_collapse, information_standard).
narrative_ontology:affects_constraint(knowledge_commons_collapse, peer_review_quality_gate).
narrative_ontology:affects_constraint(knowledge_commons_collapse, open_access_mandate_enforcement).
narrative_ontology:affects_constraint(knowledge_commons_collapse, research_institution_budget_crisis).

% DUAL FORMULATION NOTE:
% The knowledge commons collapse should be decomposed into three structurally distinct constraints per ε-invariance principle: (1) Journal subscription cost extraction (ε≈0.62) — institutional budget crisis driven by publisher pricing power. (2) Peer review gatekeeping (ε≈0.45) — quality assurance mechanism with significant theater ratio. (3) IP enforcement for research access (ε≈0.55) — legal and technical barriers to knowledge dissemination. Each has different beneficiaries (publishers vs prestige-metric gatekeepers vs patent-holding corporations), different victims, and different remediation pathways. The current story captures the integrated institutional arrangement; decomposed stories would enable more precise intervention targeting.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_commons_collapse, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
