% ============================================================================
% CONSTRAINT STORY: knowledge_commons_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_knowledge_commons_access, []).

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
 *   constraint_id: knowledge_commons_access
 *   human_readable: Knowledge Commons Access Restriction and Commodification
 *   domain: epistemic/institutional/economic
 *
 * SUMMARY:
 *   Access to knowledge is constrained by commercial publishing systems that
 *   restrict peer-reviewed research behind paywalls, creating asymmetric
 *   benefits for publishers and institutions with subscription budgets while
 *   extracting from knowledge-seekers without institutional affiliation and
 *   from researchers in resource-constrained regions. This constraint
 *   exhibits the signature of a tangled rope: it genuinely coordinates some
 *   publishing functions (peer review, copy editing, distribution) while
 *   extracting monopoly rents through copyright enforcement and bundled
 *   subscription systems. The constraint is under active challenge from
 *   open-access movements that are building alternative pathways (arXiv,
 *   PLOS, institutional mandates, preprint circulation) with explicit sunset
 *   logic — the traditional paywall system's leverage declines as open-access
 *   alternatives mature and institutional mandates enforce public funding →
 *   public access. The theater ratio (0.58) reflects that journal prestige
 *   metrics, impact factors, and peer-review rituals have become partially
 *   decoupled from their original coordination function, persisting through
 *   career path dependence and institutional inertia. Extractiveness has
 *   increased from 0.42 to 0.58 over the measurement interval as publishers
 *   have consolidated market power (fewer, larger publishers controlling more
 *   journals), bundled subscriptions have become more expensive, and
 *   open-access alternatives have not yet achieved sufficient critical mass
 *   to displace the paywall system.
 *
 * KEY AGENTS:
 *   - Knowledge Seekers Without Institutional Access: Primary victim (powerless/trapped) — face absolute barriers, no collective organizing capacity, maximum extraction
 *   - Researchers in Resource-Constrained Regions: Secondary victim (moderate/constrained) — have some exit options (preprints, ILL, OA alternatives) but significant navigation costs and research scope limitations
 *   - Well-Funded Research Institutions: Primary beneficiary (institutional/arbitrage) — can afford subscriptions, experience system as pure coordination
 *   - Commercial Publishers: Beneficiary (institutional/constrained) — benefit from extraction but depend on researcher contributions and institutional subscriber lock-in
 *   - Open Access Movement: Organized agents (organized/constrained) — building alternative pathways with sunset logic; have agency but constrained by network effects and adoption barriers
 *   - Journal Impact Factor System: Institutional actor (institutional/arbitrage) — maintains degraded prestige metrics; seen as piton due to high theater ratio relative to functional verification
 *   - Analytical Observer: Civilizational position (analytical/analytical) — sees constraint as snare that extracts from the scholarly commons itself
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(knowledge_commons_access, 0.58).
domain_priors:suppression_score(knowledge_commons_access, 0.62).
domain_priors:theater_ratio(knowledge_commons_access, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(knowledge_commons_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(knowledge_commons_access, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(knowledge_commons_access, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(knowledge_commons_access, tangled_rope).
narrative_ontology:human_readable(knowledge_commons_access, "Knowledge Commons Access Restriction and Commodification").
narrative_ontology:topic_domain(knowledge_commons_access, "epistemic/institutional/economic").

domain_priors:requires_active_enforcement(knowledge_commons_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(knowledge_commons_access, commercial_publishers).
narrative_ontology:constraint_beneficiary(knowledge_commons_access, paywalled_platforms).
narrative_ontology:constraint_beneficiary(knowledge_commons_access, institutional_license_holders).
narrative_ontology:constraint_victim(knowledge_commons_access, knowledge_seekers_without_institutional_access).
narrative_ontology:constraint_victim(knowledge_commons_access, researchers_in_resource_constrained_regions).
narrative_ontology:constraint_victim(knowledge_commons_access, scholarly_knowledge_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED KNOWLEDGE SEEKER (SNARE) — Individual learners, students, and researchers without institutional affiliation face absolute barriers to accessing peer-reviewed research. No exit option: knowledge is paywalled at $25-40 per article or $500+ annual subscription. Cannot organize collectively to negotiate better terms. Bears full extraction cost with minimal ability to exit or coordinate alternative access. Maximum experienced extraction.
constraint_indexing:constraint_classification(knowledge_commons_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCHER IN RESOURCE-CONSTRAINED REGION (TANGLED ROPE) — Institutional researchers in low-income countries or underfunded institutions experience mixed dynamics. The paywalled system coordinates some knowledge distribution through institutional subscriptions and consortium agreements, but access remains asymmetrically restricted. Exit options exist (preprints, interlibrary loan, open access alternatives) but require significant navigation effort. Partial agency — some benefit from the coordination function (institutional subscriptions enable access) alongside extraction (subscription costs are prohibitive, limiting research scope).
constraint_indexing:constraint_classification(knowledge_commons_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: WELL-FUNDED RESEARCH INSTITUTION (ROPE) — Wealthy universities and research centers with subscription budgets experience the system as pure coordination. They pay for access, which enables research and publication. The constraint solves a genuine collective action problem: who funds the peer review and publication infrastructure? Institutional arbitrage option: they can afford subscriptions and benefit from tiered access arrangements. Net beneficiary position — extraction cost is manageable relative to research benefits.
constraint_indexing:constraint_classification(knowledge_commons_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN ACCESS MOVEMENT (SCAFFOLD) — Organized agents (arXiv, PLOS, Plan S, institutional repositories, open access mandates) are building alternative knowledge distribution pathways with a clear sunset logic. Open access publication and preprint archives create parallel verification and citation systems that bypass commercial paywalls. These alternatives have low extraction cost and functional coordination. The constraint has explicit temporality: as open-access adoption increases and institutional mandates enforce public funding → public access, the traditional paywall system loses leverage. Estimated sunset: 15-25 years for mainstream disciplines (faster in physics, slower in humanities where preprint culture is weaker).
constraint_indexing:constraint_classification(knowledge_commons_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: JOURNAL IMPACT FACTOR SYSTEM (PITON) — Traditional journal prestige metrics and the citation hierarchy that gates career advancement are increasingly decoupled from access. Researchers optimize for publication in high-impact journals that are frequently paywalled, perpetuating the extraction mechanism. But the system's core function (signaling research quality) is degraded — impact factor is gamed, preprints circulate equally, and institutional mandate momentum is shifting. The theater ratio is high (journal rituals, impact metrics, peer review theater) relative to the functional verification these mechanisms provide. Maintained through institutional inertia and career path dependence, not because it optimally solves the coordination problem.
constraint_indexing:constraint_classification(knowledge_commons_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: COMMERCIAL PUBLISHER (TANGLED ROPE) — Publishers experience genuine coordination functions (organizing peer review, copy editing, distribution infrastructure) alongside extraction. Publishers benefit from bundling arrangements with institutions and from copyright-based licensing that creates monopolistic pricing. They have constrained exit options relative to their power level: they depend on researcher contributions (voluntary peer review, authorship, editorial labor) and institutional dependency locks in subscriber bases. The constraint genuinely coordinates some publishing functions while extracting monopoly rents. Perspective's d-value is lower (publisher is beneficiary with constrained exit, not trapped victim), resulting in lower experienced chi and tangled_rope classification rather than snare.
constraint_indexing:constraint_classification(knowledge_commons_access, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational perspective, knowledge access is asymmetrically restricted despite public funding of research. The scholarly commons (accumulated human knowledge) is trapped behind extraction mechanisms. The analytical position sees this as a snare: the coordination function (peer review, quality control) is contingent and could operate under alternative models (open access, decentralized peer review), while the extraction function (monopoly pricing, access restriction) is maximized. The constraint extracts from the knowledge commons itself — the collective epistemic capacity of humanity — which has no exit option and cannot organize.
constraint_indexing:constraint_classification(knowledge_commons_access, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(knowledge_commons_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(knowledge_commons_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(knowledge_commons_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(knowledge_commons_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(knowledge_commons_access, TR),
    TR >= 0.70.

:- end_tests(knowledge_commons_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Commercial publishers extract significant monopoly rents through copyright enforcement, bundled subscriptions ($500-5000+ per year for large institutions), and per-article fees ($25-40). The extraction is substantial but not maximum because: (1) some coordination functions are genuine (peer review, quality control, distribution infrastructure), (2) open-access alternatives reduce the effective monopoly power for some segments, and (3) institutional subscriptions provide some value through bundled access. The increase from 0.42 to 0.58 reflects publisher market consolidation and bundling-based rent extraction. Suppression (0.62): High. Structural barriers to exit include: copyright enforcement (legal barriers), institutional subscription bundling (high exit costs), prestige gatekeeping (career advancement requires publication in paywall journals), network effects (researchers publish where audiences read), and information asymmetries (authors don't retain publication rights). Barriers are enforced through legal (copyright), economic (subscription costs), and social (citation prestige) mechanisms. Theater ratio (0.58): Moderate-high. Journal peer review performs genuine quality gatekeeping, but this function is increasingly decoupled from prestige metrics, which are gamed and manipulated. Impact factors and journal rankings have become performative signals rather than accurate quality measures. The measurement trend shows theater increasing slightly as publishers emphasize metrics over substantive quality control. The constraint coordinates some publishing functions but with substantial theater overlay.
 *
 * PERSPECTIVAL GAP:
 *   Sharp disagreement between beneficiary and victim perspectives. Publishers and well-funded institutions perceive the system as low-extraction coordination (rope/rope). Knowledge seekers without access perceive maximum extraction with no alternatives (snare). Researchers in constrained regions perceive mixed dynamics — some coordination benefit from institutional agreements, substantial extraction from high costs (tangled rope). The gap reveals that the constraint's classification depends critically on the agent's power level and exit options: the system works well for those who can afford it, extracting maximally from those who cannot. This is diagnostic of tangled rope — asymmetric coordination where the benefits and costs are unevenly distributed.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position (beneficiary vs victim) and exit options. Beneficiary + arbitrage exit (well-funded institutions) → low d → low chi → rope. Beneficiary + constrained exit (publishers, dependent on subscription revenue) → moderate d → moderate chi → tangled rope. Victim + trapped exit (excluded knowledge seekers) → high d → high chi → snare. Victim + constrained exit (Global South researchers) → moderate-high d → moderate chi → tangled rope. Victim + analytical position (scholarly commons view) → high d → high chi → snare. The pipeline automatically computes d from these positions; the scholarly commons itself has no exit option (trapped), maximizing experienced extraction from that perspective. Publishers have constrained exit (they depend on author contributions and institutional subscriber bases) despite being nominally institutional power, so their d is higher than a pure beneficiary would have, resulting in tangled rope rather than rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the knowledge commons access constraint is genuinely both a coordination mechanism AND an extraction mechanism — not one or the other. Peer review, copy editing, and distribution infrastructure represent real coordination functions that have value. But these functions have been bundled with monopolistic pricing and copyright enforcement that extract rents from both individuals and institutions. The resolution is to recognize that the constraint's claimed type (tangled rope) reflects this hybrid: it coordinates publishing while extracting through paywalls. The mandatrophy dissolves when we ask not 'is this a rope or snare?' but 'which institutional arrangement best coordinates publishing while minimizing extraction?' Open-access alternatives (arXiv, PLOS, decentralized peer review) can provide the coordination functions at much lower extraction cost, making the paywall system's rent-extraction increasingly unjustifiable. The analytical observer's snare classification (from civilizational scope) is correct — if we evaluate the constraint at the level of the scholarly commons as a whole, it is purely extractive. But at the institutional level (publisher, university), it has coordination functions that are being co-opted into extraction mechanisms.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    open_access_adoption_trajectory,
    'What adoption rate of open-access alternatives would constitute sufficient grounds for reclassifying the constraint from tangled_rope to scaffold?',
    'Empirical tracking of OA mandate adoption, arXiv preprint circulation rates, PLOS impact factor trajectories, publisher market share shifts. Threshold: when >60% of significant research is published in OA venues with equivalent or superior prestige signals.',
    'If adoption accelerates: scaffold classification becomes dominant, sunset timeline shortens, constraint is temporary. If adoption plateaus: tangled_rope persists, extraction mechanisms remain entrenched.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_access_adoption_trajectory, empirical, 'Critical mass threshold for open-access displacement of paywall system').

omega_variable(
    preprint_verification_sufficiency,
    'Do decentralized preprint verification systems (post-publication peer review, open commentary) achieve equivalent quality assurance compared to pre-publication journal peer review?',
    'Comparative analysis of error rates, retraction rates, and citation patterns between OA preprint-first and traditional paywall-first publications. Long-term tracking of research validity across publication models.',
    'If sufficient: publishers'' claimed coordination function is not actually necessary — extraction is severable from coordination. If insufficient: journal peer review provides genuine quality gatekeeping, and extraction may be pricing for a real service.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(preprint_verification_sufficiency, empirical, 'Whether post-publication peer review provides equivalent quality control').

omega_variable(
    institutional_subscription_cost_ceiling,
    'At what subscription cost level do institutions systematically begin shifting to open-access-only research models and defunding commercial journal access?',
    'Institutional budget analysis; tracking of major university library decisions to cancel subscriptions; cost-per-article trending; institution exit rate from commercial subscription bundles.',
    'If ceiling is near current costs: tipping point is imminent, constraint''s extraction mechanism will collapse. If ceiling is much higher: institutions have high exit costs and will tolerate continued extraction despite OA alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_subscription_cost_ceiling, empirical, 'Institutional subscription cost threshold for system defection').

omega_variable(
    global_south_open_access_capacity,
    'Can resource-constrained regions build and sustain open-access publishing and verification infrastructure without dependency on wealthy-country platforms and funding?',
    'Assessment of regional OA journal capacity, institutional repository sustainability, independent preprint server viability, local peer review capacity. Measurement of OA journal survival rates and citation impact in different regions.',
    'If capacity-building succeeds: escape from paywall extraction becomes feasible for all regions. If capacity remains dependent: open-access movement risks replacing one form of structural dependency with another (transfer from commercial publishers to wealthy-country platform operators).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(global_south_open_access_capacity, empirical, 'Whether Global South can sustain independent open-access infrastructure').

omega_variable(
    government_research_funding_public_access_mandate,
    'Will major governments enforce strict public-access mandates (all publicly funded research must be openly accessible within 12 months) that compel publisher compliance?',
    'Policy tracking of Plan S variants, EU open access requirements, US federal public access mandates, and compliance enforcement mechanisms. Measurement of publisher policy changes in response to mandate enforcement.',
    'If enforced globally: constraint flips from extraction to coordination — publishers must adapt to open-access models or lose access to major funding streams. If enforcement is weak or fragmented: publishers can navigate around mandates, extraction persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(government_research_funding_public_access_mandate, preference, 'Whether public-access mandates will be enforced and effective').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(knowledge_commons_access, 0, 21).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kca_tr_t0, knowledge_commons_access, theater_ratio, 0, 0.48).
narrative_ontology:measurement(kca_tr_t7, knowledge_commons_access, theater_ratio, 7, 0.54).
narrative_ontology:measurement(kca_tr_t14, knowledge_commons_access, theater_ratio, 14, 0.58).
narrative_ontology:measurement(kca_tr_t21, knowledge_commons_access, theater_ratio, 21, 0.55).

% Extraction over time
narrative_ontology:measurement(kca_be_t0, knowledge_commons_access, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(kca_be_t7, knowledge_commons_access, base_extractiveness, 7, 0.54).
narrative_ontology:measurement(kca_be_t14, knowledge_commons_access, base_extractiveness, 14, 0.58).
narrative_ontology:measurement(kca_be_t21, knowledge_commons_access, base_extractiveness, 21, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(knowledge_commons_access, information_standard).
narrative_ontology:affects_constraint(knowledge_commons_access, academic_prestige_hierarchy).
narrative_ontology:affects_constraint(knowledge_commons_access, research_funding_allocation).
narrative_ontology:affects_constraint(knowledge_commons_access, global_knowledge_inequality).

% DUAL FORMULATION NOTE:
% Knowledge commons access is upstream of and affects multiple downstream constraints. The prestige hierarchy in academic publishing depends on paywall gatekeeping; research funding allocation is influenced by publication prestige metrics; global knowledge inequality is amplified by differential access to paywalled research. Decomposition into separate stories reflects different ε values: the publishing coordination mechanism itself (ε ≈ 0.25, rope), the paywall extraction layer (ε ≈ 0.72, snare), and the blended institutional arrangement (ε ≈ 0.58, tangled rope) are structurally distinct but causally coupled.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(knowledge_commons_access, analytical, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
