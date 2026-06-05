% ============================================================================
% CONSTRAINT STORY: academic_peer_review_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_academic_peer_review_gatekeeping, []).

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
 *   constraint_id: academic_peer_review_gatekeeping
 *   human_readable: Academic Peer Review and Journal Gatekeeping
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   Academic peer review and journal gatekeeping represents a structural
 *   extraction mechanism where researchers and institutions collectively
 *   provide essential labor (manuscript evaluation, editorial curation,
 *   quality filtering) to for-profit publishers, who then control access to
 *   that research through subscription paywalls sold back to the same
 *   institutions at escalating markups. The system persists despite
 *   widespread recognition of its dysfunction because exit is costly:
 *   researchers cannot avoid journals without sacrificing career advancement
 *   (hiring, tenure, funding all depend on publications in 'prestigious'
 *   venues), and libraries cannot cancel subscriptions without losing
 *   institutional research capacity. The constraint exhibits tangled rope
 *   characteristics (genuine coordination function bundled with asymmetric
 *   extraction) from most perspectives, but appears as pure snare (Trapped)
 *   to early-career researchers and research libraries bearing the full cost.
 *   The theater ratio (0.65) reflects that peer review's verification
 *   function has been substantially displaced by its role as
 *   career-advancement gatekeeper and legitimacy ritual. Over the 1970-2024
 *   interval, both extractiveness and theater have risen as journal
 *   concentration increased (mergers), subscription costs escalated, and
 *   academic labor became increasingly precarious.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers: Primary victims (powerless/trapped) — face publication pressure for employment and funding; cannot abandon commercial venues without career suicide
 *   - Research Universities and Libraries: Secondary victims (moderate/constrained) — absorb escalating subscription costs (30-40% annual increases); provide institutional infrastructure for peer review and editorial work
 *   - Commercial Publishers (Elsevier, Springer, Wiley, SAGE): Primary beneficiaries (institutional/arbitrage) — capture value from researcher labor; control global knowledge distribution; enjoy supernormal profit margins (35-40%)
 *   - Journal Editors: Institutional actors (institutional/constrained) — provide free labor managing peer review; maintain gatekeeping authority; vary in awareness of extraction dynamics
 *   - Open Access Movement (SPARC, funders, OA advocates): Organized challengers (organized/constrained) — recognize both coordination function and extraction; build alternative pathways (preprints, institutional repositories, open-access publishing)
 *   - Scientific Progress (Collective Good): Abstract victim (powerless/trapped) — knowledge access barriers slow innovation; inefficiencies in review process waste researcher time; premature privatization of publicly funded research
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(academic_peer_review_gatekeeping, 0.58).
domain_priors:suppression_score(academic_peer_review_gatekeeping, 0.68).
domain_priors:theater_ratio(academic_peer_review_gatekeeping, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(academic_peer_review_gatekeeping, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(academic_peer_review_gatekeeping, tangled_rope).
narrative_ontology:human_readable(academic_peer_review_gatekeeping, "Academic Peer Review and Journal Gatekeeping").
narrative_ontology:topic_domain(academic_peer_review_gatekeeping, "economic/social/technological").

domain_priors:requires_active_enforcement(academic_peer_review_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, commercial_publishers).
narrative_ontology:constraint_beneficiary(academic_peer_review_gatekeeping, journal_editors).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, academic_researchers).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, university_libraries).
narrative_ontology:constraint_victim(academic_peer_review_gatekeeping, scientific_progress).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EARLY-CAREER RESEARCHER (SNARE) — Trapped by publication requirements for employment, tenure, and grants. Must submit to commercial venues despite knowing funds flow away from science. Cannot exit without abandoning career prospects. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.95. Maximum extraction.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCH UNIVERSITY LIBRARY (SNARE) — Faces escalating journal subscription costs (30-40% annual increases) while providing free peer review labor. Cannot abandon subscriptions without losing institutional research capacity. Constrained exit — switching to open access requires faculty adoption they cannot mandate. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPEN ACCESS MOVEMENT (TANGLED ROPE) — Organized coalition (SPARC, OA advocates, funder mandates) sees genuine coordination function (peer review, curation, distribution) alongside asymmetric extraction (profit capture). Can negotiate but not fully exit. Beneficiaries: broad scientific community through accessible knowledge. Victims: commercial publisher revenue. d≈0.45, f(d)≈0.50, σ=1.2 → χ≈0.35. Hybrid classification reflects both coordination and extraction in one ecosystem.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: COMMERCIAL PUBLISHER (ROPE) — Experiences the system as pure coordination: managing peer review, handling manuscript logistics, curating quality through editorial gatekeeping, distributing globally. Publisher sees itself as solving a collective action problem (how to verify and distribute knowledge at scale). d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Net beneficiary; negative extraction indicates alignment.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL PEER REVIEW RITUAL (PITON) — The peer review process itself is highly theatrical: anonymous reviews rarely catch errors, most papers are accepted after minor revisions, reviewing is unpaid and unrewarded yet mandatory for career advancement. Theater_ratio=0.65 reflects that verification is performative while the ritual persists through institutional inertia. The process is maintained because alternatives (preprint/comment culture, post-publication review) haven't fully replaced it. d≈0.05, f(d)≈-0.12 → χ≈-0.04.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a knowledge-economy perspective, peer review does provide genuine coordination (quality curation, trust certification, distribution infrastructure) but this coordination is bundled with asymmetric extraction (profit concentration, access barriers, career dependency). The system extracts researcher labor and library budgets to fund shareholder returns rather than scientific advancement. d≈0.65, f(d)≈1.00, σ=1.2 → χ≈0.70. Tangled rope confirms the base classification.
constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(academic_peer_review_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(academic_peer_review_gatekeeping, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(academic_peer_review_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(academic_peer_review_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(academic_peer_review_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts researcher time (peer review is unpaid but required), researcher opportunity cost (time writing for commercial venues rather than research), library budgets (journals consume 30-50% of research library spend), and public research value (taxpayer-funded research locked behind paywalls). The value is 0.58 rather than 0.75+ because some extraction is offset by genuine coordination services: journals do provide peer review, curation, and distribution. But the markup (publisher margins of 35-40% vs cost of service delivery ~20%) indicates significant rent-seeking. Suppression (0.68): High. Multiple barriers lock actors into the system: (a) Career dependency — publication records determine hiring, promotion, and funding; (b) Network effects — prestige is concentrated in commercial journals (Nature, Science, Cell hierarchy); (c) Infrastructure switching costs — institutional adoption of alternatives requires coordinated faculty behavior; (d) Information asymmetry — author-side costs are visible, publisher-side costs are opaque. Theater ratio (0.65): Moderate-high. Peer review has evolved from primarily a quality filter (1970s, theater_ratio ~0.35) to a mixed legitimacy ritual and career gatekeeper. Anonymous review rarely catches errors, but manuscript desk rejections do provide real filtering. The increasing theater reflects that the process is now maintained partly for its ceremonial role (certifying researcher status) rather than purely for verification. Open-access preprints have lower theater because commenting is public and continuous rather than anonymous and post-publication.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a dramatic perspectival gap between early-career researchers (who experience pure snare — they cannot exit) and commercial publishers (who experience pure rope — solving a coordination problem). This gap is diagnostic of tangled rope classification: the same system that provides coordination (quality curation, trust certification, distribution) simultaneously extracts from researchers and libraries. The open access movement's perspective (tangled rope) acknowledges both functions. The piton perspective reveals that peer review's power has degraded — it was once a strong quality filter but is now mostly theater maintained by institutional inertia (careers depend on it, so researchers participate; journals depend on it, so they maintain it). The analytical observer's tangled rope classification confirms that the system is not pure coordination (publishers claim rope classification) nor pure extraction (some genuine services), but rather a hybrid where extraction is bundled with coordination in a way that advantages capital (publishers) and disadvantages labor (researchers).
 *
 * DIRECTIONALITY LOGIC:
 *   Early-career researchers: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction. Cannot exit publication system without ending career; must provide free peer review as condition of participation; must pay to access research they created. Research libraries: Victim + constrained → d≈0.88, f(d)≈1.35. High extraction. Can lobby publishers or negotiate consortially, but cannot fully exit without harming institutional mission; must bear escalating costs. Open access movement: Victim (field interests) + organized + constrained → d≈0.45, f(d)≈0.50. Moderate extraction. Coalition has agency and can build alternatives (arXiv, institutional repositories, OA mandates), but constrained by need to maintain dual publication strategies during transition. Commercial publishers: Beneficiary + institutional + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Publishers see themselves as solving coordination problem; have exit (ability to raise prices, consolidate, maintain margins). Analytical observer: Observes structural extraction bundled with coordination → d≈0.65, f(d)≈1.00. Moderate-high extraction effective because the coordination function cannot be unbundled without rebuilding infrastructure.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL: This constraint resolves the mandatrophy between 'peer review is essential coordination' and 'journals are extraction mechanisms' by decomposing them into analytically distinct claims. The coordination claim (peer review provides genuine quality filtering and trust certification) is true but has been substantially degraded: theater_ratio(0.65) indicates verification is ~35% functional, ~65% theatrical. The extraction claim (commercial publishers capture research value and leverage career dependency) is also true: suppression (0.68) indicates strong barriers to exit, extractiveness (0.58) indicates significant rent-seeking. Tangled rope is the correct type because BOTH claims are simultaneously true. The mandatrophy resolution: Publishers cannot be fully beneficiaries (rope type) because the system DOES extract from researchers and libraries beyond the cost of coordination services. Researchers cannot be free (snare would require trapped status, which is true for early-career but not for tenured faculty or publishers). The system is hybrid: institutional actors (publishers) benefit, precarious actors (early-career researchers, libraries) bear costs, and organized challengers (OA movement) are building exit routes. The theater ratio (0.65) indicates that peer review's role in maintaining the extraction mechanism is significant — it legitimates the gatekeeping through ceremonial legitimacy rather than functional verification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    peer_review_verification_efficacy,
    'How much quality verification is actually provided by anonymous peer review versus how much is theater for legitimacy?',
    'Longitudinal study of retracted papers and errata: correlation between peer review feedback and ultimate validity; pre/post-publication error detection rates; comparison with post-publication commentary systems',
    'If efficacy is high (>70%): peer review provides genuine coordination value, shifting toward rope classification. If efficacy is low (<30%): pure institutional theater, shifting toward snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(peer_review_verification_efficacy, empirical, 'Efficacy of peer review verification versus performative legitimacy').

omega_variable(
    commercial_publisher_necessity,
    'Is the commercial publisher''s infrastructure actually necessary for peer review, curation, and distribution, or could it be replaced by decentralized open-access systems?',
    'Comparative analysis of arXiv/bioRxiv distributed commentary systems vs journal peer review; cost analysis of publishing infrastructure; deployment of open-access publishing platforms (eLife, PLoS models)',
    'If necessary: publishers are genuine coordinators and extraction is justified service fee. If replaceable: extraction is pure rent-seeking without coordination function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commercial_publisher_necessity, empirical, 'Whether commercial publishers are structurally necessary for knowledge curation').

omega_variable(
    researcher_exit_capacity,
    'Do researchers and institutions actually have capacity to exit the commercial publishing system, or is the trapped/constrained status enforced by career incentive structures beyond the constraint''s scope?',
    'Longitudinal analysis of researchers using only preprints and open-access venues; career outcome comparison; adoption rates of open-access publishing models; funding agency mandate effectiveness',
    'If exit is possible (>50% adoption feasible): snare classification is too harsh; constraint is tangled rope or scaffold with sunset clause. If exit is structurally impossible: snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(researcher_exit_capacity, empirical, 'Whether researcher exit from commercial publishing is structurally feasible').

omega_variable(
    publisher_profit_sustainability,
    'Can commercial publishers sustain profit margins (currently 35-40%) given competition from open-access models and researcher-led alternatives?',
    'Financial analysis of publisher revenues during adoption of open-access mandates; cost projections for transitional open-access models; elasticity of institutional subscription demand',
    'If margins collapse: extraction is unsustainable and constraint will degrade. If margins persist: extraction mechanism is robust and snare aspects are durable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(publisher_profit_sustainability, empirical, 'Sustainability of commercial publisher profit margins under open-access competition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(academic_peer_review_gatekeeping, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aprg_tr_t1970, academic_peer_review_gatekeeping, theater_ratio, 1970, 0.35).
narrative_ontology:measurement(aprg_tr_t1995, academic_peer_review_gatekeeping, theater_ratio, 1995, 0.5).
narrative_ontology:measurement(aprg_tr_t2015, academic_peer_review_gatekeeping, theater_ratio, 2015, 0.6).
narrative_ontology:measurement(aprg_tr_t2024, academic_peer_review_gatekeeping, theater_ratio, 2024, 0.65).

% Extraction over time
narrative_ontology:measurement(aprg_be_t1970, academic_peer_review_gatekeeping, base_extractiveness, 1970, 0.28).
narrative_ontology:measurement(aprg_be_t1995, academic_peer_review_gatekeeping, base_extractiveness, 1995, 0.42).
narrative_ontology:measurement(aprg_be_t2015, academic_peer_review_gatekeeping, base_extractiveness, 2015, 0.52).
narrative_ontology:measurement(aprg_be_t2024, academic_peer_review_gatekeeping, base_extractiveness, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(academic_peer_review_gatekeeping, information_standard).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, academic_publishing_concentration).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, researcher_precarity).
narrative_ontology:affects_constraint(academic_peer_review_gatekeeping, open_access_infrastructure).

% DUAL FORMULATION NOTE:
% Academic peer review and journal gatekeeping decomposes into multiple structurally distinct constraints: (1) Peer review as quality mechanism (lower ε) vs peer review as career gatekeeper (higher ε); (2) Commercial publishing as distribution infrastructure (rope) vs commercial publishing as extraction mechanism (snare/tangled rope). This story addresses the integrated system with ε=0.58 (tangled rope). Upstream constraints (researcher_precarity, academic_publishing_concentration) establish the conditions enabling high suppression; downstream constraints (open_access_infrastructure) represent alternative coordination mechanisms competing for adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(academic_peer_review_gatekeeping, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
