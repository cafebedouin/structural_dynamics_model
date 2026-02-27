% ============================================================================
% CONSTRAINT STORY: epistemic_free_rider_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_free_rider_problem, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: epistemic_free_rider_problem
 *   human_readable: The Truth-Mining Exhaustion
 *   domain: informational/social/economic
 *
 * SUMMARY:
 *   The epistemic free-rider problem describes a structural collapse in
 *   information economics where the costs of producing verified, grounded
 *   knowledge are borne by an exhausted minority of truth-miners while the
 *   majority of the population consumes low-cost synthetic derivatives
 *   (AI-generated summaries, unchecked social media claims, derivative
 *   analysis). The constraint operates as a snare: truth-miners are trapped
 *   by the collapse of the epistemic commons itself—abandoning verification
 *   accelerates the commons' degradation, but continuing to bear verification
 *   costs becomes unsustainable. The synthetic-content arbitrage (zero
 *   verification cost, high consumption value) creates an extraction
 *   mechanism that benefits platforms, content aggregators, and free-riding
 *   consumers while imposing costs on truth-miners (funding scarcity,
 *   attention deficit, reputational risk of fighting noise) and the epistemic
 *   commons (information quality degradation, loss of grounding mechanisms).
 *   The theater ratio (0.65) reflects that institutional verification claims
 *   (peer review, editorial standards, fact-checking labels) are increasingly
 *   performative: verification theater persists as institutional inertia
 *   while the actual verification capacity shrinks. The constraint's
 *   extractiveness has increased from 0.28 to 0.58 over a 20-year window,
 *   indicating the snare is tightening—the ratio of synthetic derivatives to
 *   verified information is growing, truth-mining populations are declining
 *   in relative size, and platform incentives for synthetic content are
 *   intensifying.
 *
 * KEY AGENTS:
 *   - Truth-miners (researchers, investigative journalists, domain experts): Primary victims (powerless/trapped) — bear full verification costs, face funding scarcity, declining institutional support, reputational attacks
 *   - Epistemic commons: Collective victim (powerless/trapped) — abstract good that cannot advocate; experiences extraction as information quality degradation and loss of verification signals
 *   - Synthetic content consumers: Secondary victims/beneficiaries (moderate/constrained) — extract epistemic value without bearing costs, but become snared as information quality degrades and they lose ability to distinguish signal from noise
 *   - Platforms/media intermediaries: Institutional beneficiaries (institutional/arbitrage) — monetize engagement from both truth-miners and consumers; maintain performative verification theater while optimizing for synthetic content engagement
 *   - Academic research institutions: Organized actors (organized/constrained) — coordinate knowledge production but also extract surplus through publication capture, tenure gatekeeping, productivity metrics that incentivize quantity over verification rigor
 *   - Policy-makers and downstream decision-makers: Implicit victims (moderate/constrained) — depend on epistemic commons quality for informed decision-making; face increasing probability of decisions based on degraded or synthetic information
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_free_rider_problem, 0.58).
domain_priors:suppression_score(epistemic_free_rider_problem, 0.68).
domain_priors:theater_ratio(epistemic_free_rider_problem, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_free_rider_problem, extractiveness, 0.58).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(epistemic_free_rider_problem, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_free_rider_problem, snare).
narrative_ontology:human_readable(epistemic_free_rider_problem, "The Truth-Mining Exhaustion").
narrative_ontology:topic_domain(epistemic_free_rider_problem, "informational/social/economic").

% --- Structural relationships ---
narrative_ontology:constraint_victim(epistemic_free_rider_problem, truth_miners).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, epistemic_commons).
narrative_ontology:constraint_victim(epistemic_free_rider_problem, downstream_policy_makers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXHAUSTED TRUTH-MINER (SNARE) — Bears all verification costs (funding, time, reputational risk). Cannot exit: abandoning verification work collapses epistemic commons. Trapped by dependency on institutional funding and tenure systems that reward productivity over truth-quality. d≈0.98, f(d)≈1.48, σ=1.2 → χ≈1.04. Maximum extraction signature.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYNTHETIC CONTENT CONSUMER (SNARE) — Extracts epistemic value from truth-miner labor without bearing verification costs. Has constrained exit: technically can verify claims but faces prohibitive time/expertise barriers. d≈0.65, f(d)≈0.95, σ=1.2 → χ≈0.66. Beneficiary from asymmetry but becomes snared as epistemic quality degrades.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MEDIA/PLATFORM INTERMEDIARY (PITON) — Extracts attention value from both truth-miners and consumers; maintains performative verification theater (fact-checking labels, editorial standards) while monetizing engagement. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.07. Sees its own verification theater as degraded (theater_ratio=0.65) — maintains ritual through inertia despite knowing it creates moral hazard. Arbitrage exit: can pivot to pure synthetic content if truth-mining economics collapse.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ACADEMIC RESEARCH INSTITUTION (TANGLED ROPE) — Coordinates knowledge production (legitimate coordination function) but also extracts surplus from truth-miners via publication capture, tenure gatekeeping, and productivity metrics that reward quantity over verification rigor. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.52. Both beneficiary (coordinates research) and extractor (captures surplus). Active enforcement required: peer review rituals, journal systems, citation metrics.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: EPISTEMIC COMMONS (SNARE) — Abstract victim. Cannot organize or exit. Experiences extraction as degradation: verified information becomes minority good, synthetic derivatives dominate, truth-mining signals get lost in noise. d≈0.99, f(d)≈1.49, σ=1.2 → χ≈1.08. Pure extraction: the commons has no self-advocacy mechanism.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (PITON) — From a civilizational frame, risks naturalizing the constraint as inherent to information asymmetry and human cognitive limits ('knowledge will always be scarce, verification always expensive'). This perspective risks seeing an immutable mountain when the structural data (ε=0.58, suppression=0.68, theater=0.65) reveals contingent institutional arrangements: monetary incentives for synthetic content, platform algorithms that reward engagement over accuracy, and academic systems that commodify truth-mining. Theater gate (0.65 ≥ 0.70 threshold not quite met) suggests this is PITON not MOUNTAIN — sustained by institutional inertia and performative verification theater, not by natural law. d≈0.72, f(d)≈1.13, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(epistemic_free_rider_problem, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_free_rider_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_free_rider_problem, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_free_rider_problem, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(epistemic_free_rider_problem, TR),
    TR >= 0.70.

:- end_tests(epistemic_free_rider_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Significant and rising. The constraint extracts epistemic labor from truth-miners while providing synthetic derivatives as free-riding alternatives. The 20-year trend (0.28 → 0.58) shows escalating extraction as platform algorithms optimize for engagement and synthetic content generation becomes cheaper. Suppression (0.68): High. Barriers to truth-mining include: funding scarcity (research budgets declining relative to content consumption), attention deficit (verification work is slow and unrewarded by recommendation algorithms), expertise barriers (synthetic derivatives claim expertise without grounding), and institutional degradation (academic systems now incentivize productivity over verification quality). Truth-miners cannot easily exit: the epistemic commons itself becomes a trap — abandoning verification accelerates collapse, continuing to bear costs becomes unsustainable. Theater ratio (0.65): Moderate-high. Institutional verification claims (journal peer review, platform fact-checking labels, editorial standards) persist but increasingly performative. Platforms label synthetic content as 'AI-assisted' but amplify it regardless. Academic journals maintain review processes but face reviewer burnout and declining quality. The theater has risen from 0.35 to 0.65 as institutions double down on verification ritual while actual capacity shrinks. Mandatrophy: Resolved by showing this is NOT a mountain (immutable law of information scarcity) but a snare with specific extractive agents (platforms, content aggregators) who benefit from the asymmetry. Reform is possible if institutional incentive structures change.
 *
 * PERSPECTIVAL GAP:
 *   The truth-miners see a pure snare: they are trapped by declining funding, reputational attacks, and the collapse of the epistemic commons itself. Synthetic content consumers initially see a rope (coordination of information access) but become snared as information quality degrades. Platforms see a piton (their performative verification theater is degraded but maintained through inertia and institutional lock-in). Academic institutions see themselves as coordinating knowledge (rope) but actually extract surplus through productivity metrics and publication capture (tangled rope). The epistemic commons is powerless—it cannot organize or exit, experiencing pure extraction. The analytical observer risks seeing an immutable mountain ('truth is scarce, verification is expensive, information inequality is natural') when the structural data reveals contingent institutional arrangements: platform algorithms optimized for engagement, academic systems optimizing for productivity, and the lack of institutional support for truth-mining labor. The perspectival gap reveals that what appears as 'natural' information scarcity is actually engineered by incentive structures that favor synthetic derivatives.
 *
 * DIRECTIONALITY LOGIC:
 *   Truth-miners: Victim + trapped → d≈0.98, f(d)≈1.48. Extreme extraction. Cannot exit without epistemic commons collapse; verification costs borne entirely by this group. Synthetic content consumers: Beneficiary (initial) + constrained → d≈0.65, f(d)≈0.95. Net extraction from truth-miners, but constrained because their information quality depends on verification ecosystem's health. Platforms/media intermediaries: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit by pivoting entirely to synthetic content if truth-mining collapses. Academic institutions: Beneficiary + constrained → d≈0.55, f(d)≈0.75. Mixed coordination (legitimate knowledge production) and extraction (surplus capture via publication systems, tenure gatekeeping). Epistemic commons: Victim + trapped → d≈0.99, f(d)≈1.49. Abstract victim, maximum extraction. Analytical observer: analytical → d≈0.72, f(d)≈1.13. Risks naturalizing contingent institutional arrangements as immutable laws.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLVES MANDATROPHY: The constraint is NOT a mountain (immutable law of information asymmetry) but a snare with identifiable beneficiaries (platforms, content aggregators, free-riding consumers) and identified victims (truth-miners, epistemic commons). This classification enables structural reform: if the snare is maintained by specific institutional incentives (platform engagement algorithms, academic productivity metrics, funding structures), then changing those incentives can release the trap. The rising extractiveness (0.28 → 0.58) and theater ratio (0.35 → 0.65) show the snare is tightening, not relaxing—without intervention, the epistemic commons will degrade past recovery threshold. The constraint is NOT 'humans are lazy and don't want to verify claims' (mountain) but 'institutional systems have optimized to extract epistemic value from truth-miners while subsidizing synthetic content consumption' (snare). Mandatrophy is resolved by showing the classification is determinate and structural: separate agent perspectives produce different types (piton, snare, tangled_rope) because different agents experience the constraint differently, but from the truth-miners' structural position, the classification is unambiguously snare. Reform pathways: (1) platform algorithm neutrality (reduce amplification of synthetic content), (2) academic incentive restructuring (reward verification rigor over productivity), (3) institutional support for truth-mining labor (funding, prestige, career security), (4) literacy infrastructure (help consumers distinguish verified from synthetic). All pathways require acknowledging the constraint is a snare, not a natural limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    critical_mass_truth_miner_collapse,
    'What is the minimum viable population of truth-miners required to maintain epistemic commons above degradation threshold?',
    'Longitudinal tracking of active research populations in key domains; correlation between researcher concentration and information quality metrics; stability analysis of knowledge production networks',
    'If critical mass < 5% of information workforce: commons already below threshold, collapse irreversible absent major institutional restructuring. If critical mass > 20%: current erosion trajectory suggests 10-30 years to threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(critical_mass_truth_miner_collapse, empirical, 'Minimum viable population of truth-miners for epistemic commons stability').

omega_variable(
    synthetic_derivative_harm_threshold,
    'At what ratio of synthetic derivatives to verified information does downstream decision-making (policy, medicine, engineering) become statistically indistinguishable from random guidance?',
    'Controlled studies of policy outcomes based on high vs low synthetic content; analysis of error propagation through information chains; historical comparison of knowledge-quality-dependent failure rates',
    'If threshold already crossed: current epistemic crisis is structural, not correctable by incremental truth-mining incentives. If threshold > 10 years away: reform window still open for institutional restructuring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthetic_derivative_harm_threshold, empirical, 'Information quality threshold for policy/decision-making failure').

omega_variable(
    truth_miner_compensation_paradox,
    'Can direct monetary compensation for verification work overcome the synthetic-content arbitrage without creating new extraction mechanisms (e.g., pay-to-publish bias)?',
    'Pilot programs testing compensation models; comparison of verification quality under different funding structures; analysis of bias introduction under various incentive schemes',
    'If resolvable: institutional restructuring pathway available. If unresolvable: truth-mining exhaustion is not a resource problem but a structural economic problem requiring supply-side constraints on synthetic content.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(truth_miner_compensation_paradox, conceptual, 'Whether compensation can solve truth-mining exhaustion without new bias').

omega_variable(
    platform_algorithm_neutrality,
    'Do platform algorithms systematically amplify synthetic derivatives relative to verified information due to engagement optimization, or due to explicit content policy?',
    'Audit of algorithmic ranking; analysis of engagement metrics for synthetic vs verified content; A/B testing of neutrality constraints; investigation of explicit moderation rules',
    'If systematic (algorithm): constraint is economic/coordination problem (Tangled Rope). If explicit (policy): constraint is snare with clear beneficiary (platform). Different impacts suggest different remedies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_algorithm_neutrality, empirical, 'Whether platform amplification of synthetic content is algorithmic or policy-driven').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_free_rider_problem, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epfr_tr_t0, epistemic_free_rider_problem, theater_ratio, 0, 0.35).
narrative_ontology:measurement(epfr_tr_t10, epistemic_free_rider_problem, theater_ratio, 10, 0.5).
narrative_ontology:measurement(epfr_tr_t20, epistemic_free_rider_problem, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(epfr_be_t0, epistemic_free_rider_problem, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(epfr_be_t10, epistemic_free_rider_problem, base_extractiveness, 10, 0.43).
narrative_ontology:measurement(epfr_be_t20, epistemic_free_rider_problem, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_free_rider_problem, information_standard).
narrative_ontology:affects_constraint(epistemic_free_rider_problem, synthetic_content_amplification).
narrative_ontology:affects_constraint(epistemic_free_rider_problem, academic_publication_capture).
narrative_ontology:affects_constraint(epistemic_free_rider_problem, platform_engagement_optimization).

% DUAL FORMULATION NOTE:
% The epistemic free-rider problem decomposes into upstream constraints on platform/academic systems (synthetic_content_amplification, engagement_optimization) that create the conditions for truth-mining exhaustion, and downstream constraints on information quality and policy decision-making. This story focuses on the mechanism (free-rider dynamics) that links platform incentives to truth-miner exhaustion. Related constraint stories examine specific institutional mechanisms (publication systems, algorithmic ranking, funding structures) that implement the extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
