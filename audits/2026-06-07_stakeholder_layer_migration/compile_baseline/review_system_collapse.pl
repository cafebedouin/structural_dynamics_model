% ============================================================================
% CONSTRAINT STORY: review_system_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_review_system_collapse, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: review_system_collapse
 *   human_readable: AI Research Review System Collapse Under Market Pressure
 *   domain: science_policy/professional_ethics/technology_governance
 *
 * SUMMARY:
 *   The collapse of peer review integrity under market pressure in AI
 *   research represents a structural transformation where commercial
 *   announcement timelines have become incompatible with traditional
 *   verification processes. Companies announce results via press releases on
 *   market-relevant timelines (days to weeks) to capture investor attention,
 *   competitive advantage, and talent recruitment benefits. Traditional peer
 *   review operates on 3-6 month timelines and cannot gate information flow
 *   when press releases have already shaped public discourse, policy
 *   decisions, and investment allocation. The constraint exhibits rising
 *   extraction over the interval as the press-release-first model normalized:
 *   early adopters faced reputational risk, but as the practice spread,
 *   companies that maintained traditional review-first discipline faced
 *   competitive disadvantage. Theater ratio rises as journals continue
 *   performing review rituals on papers whose claims have already been
 *   accepted or rejected by the market. Suppression requirement rises as the
 *   system actively prevents recovery: academic researchers who criticize
 *   unvetted claims face career risk, journals that reject press-released
 *   papers lose relevance, and funding agencies that require pre-publication
 *   review lose access to cutting-edge work.
 *
 * KEY AGENTS:
 *   - Commercial AI Companies: Primary beneficiary (institutional/arbitrage) — capture market timing advantages, investor attention, talent recruitment, and competitive positioning through press-release-first strategy
 *   - Peer Review Integrity: Primary victim (powerless/trapped) — abstract epistemic commons with no exit option; bears full cost of premature claims and verification collapse
 *   - Academic Researchers: Secondary victim (moderate/constrained) — trapped by career dependence on publication metrics and professional review obligations; cannot exit without abandoning career investment
 *   - University Research Labs: Mixed position (institutional/constrained) — benefit from industry partnerships but bear costs of competing with unvetted claims and defending eroding standards
 *   - Open Science Reformers: Organized coalition (organized/mobile) — building alternative rapid verification pathways (preprint review, open benchmarks, adversarial collaboration)
 *   - Traditional Journal System: Degraded gatekeeper (institutional/constrained) — maintains review rituals that no longer control information flow; persists through prestige economy and institutional inertia
 *   - Institutional Investors: Secondary beneficiary (institutional/arbitrage) — benefit from early access to technical claims for investment decisions, even when claims are unvetted
 *   - Executive Leadership: Secondary beneficiary (powerful/arbitrage) — benefit from stock price impacts of announcements regardless of subsequent verification outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(review_system_collapse, 0.68).
domain_priors:suppression_score(review_system_collapse, 0.72).
domain_priors:theater_ratio(review_system_collapse, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(review_system_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(review_system_collapse, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(review_system_collapse, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(review_system_collapse, snare).
narrative_ontology:human_readable(review_system_collapse, "AI Research Review System Collapse Under Market Pressure").
narrative_ontology:topic_domain(review_system_collapse, "science_policy/professional_ethics/technology_governance").

domain_priors:requires_active_enforcement(review_system_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(review_system_collapse, commercial_ai_companies).
narrative_ontology:constraint_beneficiary(review_system_collapse, institutional_investors).
narrative_ontology:constraint_beneficiary(review_system_collapse, executive_leadership).
narrative_ontology:constraint_victim(review_system_collapse, peer_review_integrity).
narrative_ontology:constraint_victim(review_system_collapse, academic_researchers).
narrative_ontology:constraint_victim(review_system_collapse, public_epistemic_commons).
narrative_ontology:constraint_vindicates(review_system_collapse, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(review_system_collapse, innovation_velocity_imperative).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PEER REVIEW INTEGRITY (SNARE) — The epistemic commons cannot exit the collapse. Traditional review timelines (3-6 months) are structurally incompatible with market announcement cycles (days to weeks). No alternative verification pathway exists at scale. Maximum extraction: the review system bears full cost of premature claims while companies capture market benefits.
constraint_indexing:constraint_classification(review_system_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ACADEMIC RESEARCHERS (SNARE) — Constrained by career dependence on publication and citation metrics. Cannot refuse to review (professional obligation) but reviewing unvetted press-release claims diverts effort from legitimate research. Exit options limited: leaving academia means abandoning career investment; staying means participating in degraded system. High extraction despite moderate power.
constraint_indexing:constraint_classification(review_system_collapse, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: COMMERCIAL AI COMPANIES (ROPE) — Primary beneficiaries. Press-release-first strategy solves genuine coordination problem: communicating technical advances to investors, partners, and customers on market-relevant timelines. Peer review delay (months) is incompatible with competitive dynamics and funding cycles. Experiences constraint as pure coordination with negligible extraction.
constraint_indexing:constraint_classification(review_system_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: UNIVERSITY RESEARCH LABS (TANGLED ROPE) — Mixed position. Benefit from industry partnerships and access to computational resources, but also bear costs of competing with unvetted claims and defending traditional review standards. Constrained exit: cannot fully adopt press-release model without losing academic legitimacy, cannot ignore it without losing competitive position. Genuine coordination function (industry collaboration) coupled with asymmetric extraction (epistemic standards erosion).
constraint_indexing:constraint_classification(review_system_collapse, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: OPEN SCIENCE REFORMERS (SCAFFOLD) — Organized coalition (preprint servers, registered reports, reproducibility initiatives) building alternative verification pathways. See press-release collapse as temporary crisis driving adoption of rapid preprint review, open benchmarks, and adversarial collaboration protocols. Sunset logic: as these mechanisms mature, press-release-first loses informational advantage. Estimated timeline: 5-10 years for norms to stabilize.
constraint_indexing:constraint_classification(review_system_collapse, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: TRADITIONAL JOURNAL SYSTEM (PITON) — Maintains review rituals that no longer gate information flow. Papers appear months after press releases have already shaped public discourse, policy, and investment. Review process persists through institutional inertia and prestige economy, not because it controls epistemic access. High theater ratio: the review is performed but its gatekeeping function has atrophied.
constraint_indexing:constraint_classification(review_system_collapse, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint exhibits both genuine coordination (rapid technical communication enables distributed research) and substantial extraction (epistemic standards collapse under market pressure). The press-release-first model solves a real coordination problem but does so by externalizing verification costs onto the academic commons. Requires active enforcement: companies must suppress alternative verification timelines and maintain market pressure to prevent review system recovery.
constraint_indexing:constraint_classification(review_system_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(review_system_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(review_system_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(review_system_collapse, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(review_system_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(review_system_collapse, TR),
    TR >= 0.70.

:- end_tests(review_system_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. Commercial AI companies capture substantial benefits (market timing, investor attention, talent recruitment, competitive positioning) by externalizing verification costs onto the academic commons. The extraction is not total (0.68 rather than 0.85+) because some genuine coordination function exists — rapid technical communication does enable distributed research progress. But the coordination could occur through faster preprint review rather than press-release-first, so much of the extraction is avoidable. Suppression (0.72): High. Multiple mechanisms suppress alternatives and prevent recovery: (1) Academic researchers who criticize unvetted claims face reputational and career risk in an industry-dependent funding environment. (2) Journals that reject press-released papers lose relevance and submissions. (3) Funding agencies that require pre-publication review lose access to cutting-edge work and competitive researchers. (4) Market pressure creates prisoner's dilemma: individual companies cannot unilaterally slow announcements without competitive disadvantage. (5) Younger researchers socialized into press-release-first culture may lack capacity to restore traditional standards even if incentives changed. Theater ratio (0.58): Moderate-high. Traditional peer review continues but its gatekeeping function has atrophied. Papers appear months after press releases have already shaped discourse, policy, and investment. Review catches some errors but cannot prevent premature claims from having impact. The review is performed (maintaining prestige economy and institutional legitimacy) but its verification function is substantially degraded. Theater has risen over the interval as the gap between announcement and publication widened and as the proportion of announced results never published increased.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how structural position determines classification. Commercial AI companies experience pure coordination (Rope) — they are solving the genuine problem of communicating advances on market timelines, and from their position the extraction is invisible (externalized onto academic commons). Academic researchers experience extraction (Snare) — they are trapped by career dependence and professional obligations, bearing costs of reviewing unvetted claims while companies capture benefits. University research labs experience mixed coordination and extraction (Tangled Rope) — they benefit from industry partnerships but also bear costs of competing with unvetted claims. Open science reformers see temporary problem with sunset (Scaffold) — alternative verification pathways are maturing and will eventually eliminate press-release advantage. Traditional journals see degraded ritual (Piton) — review persists through inertia but gatekeeping function has atrophied. Analytical observer sees both genuine coordination function and substantial extraction (Tangled Rope) — the press-release model solves a real problem but does so by externalizing verification costs, and it requires active suppression to prevent review system recovery. The perspectival gap is not about disagreement over facts but about which costs and benefits are visible from each structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Commercial AI companies are primary beneficiaries with arbitrage-level exit options — they can choose press-release-first or traditional review-first based on strategic calculation, and they benefit asymmetrically from the current equilibrium. Engine derives d near 0.0 (full beneficiary) → f(d) strongly negative → negative chi (subsidy). Peer review integrity is powerless/trapped victim — an abstract collective good that cannot exit, organize, or defend itself. Engine derives d = 1.0 (full target) → f(d) = 1.0 → chi equals base extractiveness amplified by global scope. Academic researchers are moderate/constrained victims — they have some agency (can choose research directions, can refuse some review requests) but are structurally trapped by career dependence on publication metrics and professional obligations. Engine derives d toward high end (0.7-0.8) → f(d) substantial → high chi. University research labs are institutional/constrained with mixed beneficiary-victim status — they benefit from industry partnerships but bear costs of epistemic standards erosion. Engine derives d toward middle (0.4-0.5) → f(d) moderate → moderate chi, consistent with tangled_rope classification. Open science reformers are organized/mobile — they have exit options (building alternative systems) and collective agency. Engine derives d toward low-middle (0.3-0.4) → f(d) low-moderate → low chi, consistent with scaffold classification where extraction is temporary and surmountable. Traditional journal system is institutional/constrained — maintains degraded process through inertia; not primary victim (journals still publish and collect prestige) but also not beneficiary. Engine derives d toward middle → moderate chi, but piton classification comes from theater gate rather than chi magnitude.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by showing that snare classification is perspectival rather than absolute. From the position of peer review integrity (powerless/trapped), the constraint is pure extraction — an epistemic commons collapse with no exit and no benefit. From the position of commercial AI companies (institutional/arbitrage), the constraint is pure coordination — rapid communication on market timelines with negligible experienced extraction. From the analytical position (analytical/analytical), the constraint is tangled rope — genuine coordination function coupled with substantial extraction that requires active enforcement to maintain. The mandatrophy question 'Is this coordination or extraction?' has no single answer — it depends on which structural position you measure from. The claimed_type (snare) reflects the authoring perspective's judgment that the extraction dominates and that the coordination story is partly cover, but the engine will compute different types from different perspectives, and that divergence is the measurement the framework exists to take. A constraint whose perspectives all agree is either uniform-type (mountain or rope only) or under-analyzed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_timeline_compatibility,
    'Is traditional peer review timeline (3-6 months) structurally incompatible with AI development velocity, or is the incompatibility constructed by market incentives?',
    'Historical comparison: review timelines in other fast-moving fields (genomics, particle physics) that maintained review integrity; analysis of whether technical complexity genuinely requires months or whether delay is institutional overhead',
    'If structurally incompatible: press-release model is inevitable adaptation (Rope from more perspectives). If constructed: market pressure is extracting from epistemic commons (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_timeline_compatibility, empirical, 'Whether review timeline incompatibility is structural or constructed').

omega_variable(
    preprint_review_sufficiency,
    'Can rapid preprint review with open benchmarks provide verification quality comparable to traditional peer review for AI systems claims?',
    'Comparison of error detection rates: preprint community review vs traditional journal review for same AI papers; tracking of claims that survived preprint scrutiny but failed replication vs claims that passed journal review but failed replication',
    'If sufficient: scaffold perspective confirmed, sunset is real. If insufficient: open science alternative cannot replace traditional review, collapse is permanent extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(preprint_review_sufficiency, empirical, 'Whether preprint review can match traditional review quality').

omega_variable(
    market_pressure_necessity,
    'Is market-timeline announcement necessary for AI company survival, or is it a coordination equilibrium that could be collectively abandoned?',
    'Game-theoretic analysis of announcement timing as prisoner''s dilemma; historical cases of industries that successfully coordinated slower announcement norms; investor response to companies that delay announcements for verification',
    'If necessary: companies are trapped in competitive dynamic (reduces beneficiary culpability). If equilibrium: companies are choosing extraction over verification (increases beneficiary culpability).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_pressure_necessity, conceptual, 'Whether market pressure is structural necessity or coordination failure').

omega_variable(
    epistemic_commons_recovery,
    'Can peer review integrity recover once press-release-first becomes normalized, or is the degradation irreversible?',
    'Historical analysis of epistemic commons recovery after similar collapses (cold fusion, polywater); identification of mechanisms that enable or prevent norm restoration; tracking of whether younger researchers socialized into press-release-first culture retain capacity for traditional review standards',
    'If recoverable: temporary scaffold dynamics possible. If irreversible: permanent snare, extraction becomes structural feature of AI research ecosystem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(epistemic_commons_recovery, empirical, 'Whether epistemic commons can recover from review collapse').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(review_system_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(review_collapse_theater_t0, review_system_collapse, theater_ratio, 0, 0.25).
narrative_ontology:measurement(review_collapse_theater_t2, review_system_collapse, theater_ratio, 2, 0.35).
narrative_ontology:measurement(review_collapse_theater_t4, review_system_collapse, theater_ratio, 4, 0.45).
narrative_ontology:measurement(review_collapse_theater_t6, review_system_collapse, theater_ratio, 6, 0.52).
narrative_ontology:measurement(review_collapse_theater_t8, review_system_collapse, theater_ratio, 8, 0.56).
narrative_ontology:measurement(review_collapse_theater_t10, review_system_collapse, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(review_collapse_extract_t0, review_system_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(review_collapse_extract_t2, review_system_collapse, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(review_collapse_extract_t4, review_system_collapse, base_extractiveness, 4, 0.55).
narrative_ontology:measurement(review_collapse_extract_t6, review_system_collapse, base_extractiveness, 6, 0.62).
narrative_ontology:measurement(review_collapse_extract_t8, review_system_collapse, base_extractiveness, 8, 0.66).
narrative_ontology:measurement(review_collapse_extract_t10, review_system_collapse, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(review_collapse_suppress_t0, review_system_collapse, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(review_collapse_suppress_t2, review_system_collapse, suppression_requirement, 2, 0.5).
narrative_ontology:measurement(review_collapse_suppress_t4, review_system_collapse, suppression_requirement, 4, 0.58).
narrative_ontology:measurement(review_collapse_suppress_t6, review_system_collapse, suppression_requirement, 6, 0.65).
narrative_ontology:measurement(review_collapse_suppress_t8, review_system_collapse, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(review_collapse_suppress_t10, review_system_collapse, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(review_system_collapse, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is structurally independent — it describes a general epistemic commons collapse pattern rather than a specific technical claim. It could be instantiated in other domains (pharmaceutical trials, climate science, financial modeling) with similar market pressure dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
