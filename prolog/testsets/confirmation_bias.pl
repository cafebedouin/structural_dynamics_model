% ============================================================================
% CONSTRAINT STORY: confirmation_bias
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_confirmation_bias, []).

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
 *   constraint_id: confirmation_bias
 *   human_readable: Confirmation Bias (Socially Amplified)
 *   domain: social/cognitive/technological
 *
 * SUMMARY:
 *   Confirmation bias in social systems operates as a multi-layer constraint
 *   with distinct perspectives depending on the observer's structural
 *   position. At the individual cognitive level, selective attention and
 *   belief-consistent interpretation are unavoidable features of bounded
 *   rationality — no agent can process all information or operate without
 *   prior structure. This constraint becomes socially amplified and
 *   extractive when algorithmic systems, institutional incentives, and
 *   network effects create asymmetric suppression of contrary views and
 *   asymmetric costs for changing beliefs. The constraint exhibits the full
 *   six-type spectrum: the epistemic commons and minority viewpoints
 *   experience snare-like suppression; individual belief maintainers
 *   experience tangled-rope mixing (psychological benefit of confirmatory
 *   closure with epistemic cost); algorithms and institutions experience
 *   rope-like or tangled-rope benefit from consensus maintenance; the
 *   rationalist ideal persists as piton-like performance; the analytical
 *   observer risks seeing inevitable cognitive architecture (mountain) when
 *   actually observing socially contingent amplification. The theater ratio
 *   (0.68) reflects how institutions maintain the facade of unbiased
 *   reasoning and rational deliberation while structurally encoding
 *   confirmatory processes. The rising extractiveness trajectory (0.32 → 0.58
 *   over 20 years) tracks algorithmic amplification: as social media
 *   algorithms became more sophisticated in engagement optimization and as
 *   information networks became more densely connective, confirmation bias
 *   effects amplified from baseline cognition into structural extraction.
 *
 * KEY AGENTS:
 *   - Epistemic Commons: Primary victim (powerless/trapped) — abstract collective good that cannot organize to resist bias accumulation; bears epistemic pollution costs
 *   - Minority Viewpoint Holders: Primary victim (powerless/trapped) — face suppression through algorithmic filtering, social ostracism, and systematic unfavorable interpretation; cannot exit without abandoning identity
 *   - Individual Belief Maintainers: Secondary agent (moderate/constrained) — benefit from psychological fluency and community belonging (tangled rope); also bear costs of poor decisions and polarization
 *   - Social Media Algorithms: Primary beneficiary (institutional/arbitrage) — optimize for engagement by amplifying confirmationist content; capture attention value and user time
 *   - Institutional Consensus Enforcers: Secondary beneficiary (organized/constrained) — maintain institutional narratives and suppress costly dissent management; constrained by empirical reality and legitimacy requirements
 *   - Cognitive-Rationalist Ideology: Theater performer (institutional/arbitrage) — persists as ideal despite psychological evidence; masks actualized bias in institutions through performative objectivity claims
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing socially contingent amplification as inevitable cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(confirmation_bias, 0.58).
domain_priors:suppression_score(confirmation_bias, 0.62).
domain_priors:theater_ratio(confirmation_bias, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(confirmation_bias, extractiveness, 0.58).
narrative_ontology:constraint_metric(confirmation_bias, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(confirmation_bias, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(confirmation_bias, tangled_rope).
narrative_ontology:human_readable(confirmation_bias, "Confirmation Bias (Socially Amplified)").
narrative_ontology:topic_domain(confirmation_bias, "social/cognitive/technological").

domain_priors:requires_active_enforcement(confirmation_bias).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(confirmation_bias, belief_maintainers).
narrative_ontology:constraint_beneficiary(confirmation_bias, algorithmic_content_curators).
narrative_ontology:constraint_beneficiary(confirmation_bias, institutional_consensus_enforcers).
narrative_ontology:constraint_victim(confirmation_bias, epistemic_commons).
narrative_ontology:constraint_victim(confirmation_bias, minority_viewpoints).
narrative_ontology:constraint_victim(confirmation_bias, reality_alignment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMIC COMMONS (SNARE) — The abstract collective good of shared reality and reliable information cannot organize or exit. Confirmation bias systematically degrades information quality; the commons bears extraction with no voice or exit option. Each agent's local bias cascades into collective epistemic pollution.
constraint_indexing:constraint_classification(confirmation_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MINORITY VIEWPOINT HOLDERS (SNARE) — Bearers of non-consensus views face suppression through algorithmic filtering, social rejection, and information asymmetry. Cannot exit without abandoning identity. Experience maximum extraction: visibility suppressed, interpretations systematically disadvantaged, recall by majorities biased toward dismissal.
constraint_indexing:constraint_classification(confirmation_bias, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: SOCIAL MEDIA ALGORITHMS (ROPE) — Optimize for engagement by amplifying confirmationist content. Experience the constraint as pure coordination: matching users to confirming content solves the engagement problem. Net beneficiary — extraction flows toward them through attention capture. The algorithm has arbitrary exit (can be modified) but current instantiation benefits from confirmation bias.
constraint_indexing:constraint_classification(confirmation_bias, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INDIVIDUAL BELIEF MAINTAINERS (TANGLED ROPE) — Ordinary agents benefit from confirmation bias (cognitive fluency, identity protection) but also bear costs (poor decision-making, polarization, relationship breakdown). Constrained exit — can reduce bias through deliberate effort but at psychological cost. Coordination benefit: shared belief communities provide belonging and meaning. Extraction cost: locked into echo chambers, denied contrary information.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INSTITUTIONAL CONSENSUS ENFORCERS (TANGLED ROPE) — Scientific institutions, regulatory bodies, media organizations benefit from consensus maintenance (reduces dissent-management costs, protects institutional reputation). But also constrained: must respond to minority views empirically, face legitimacy threats when confirmation bias is exposed. Active enforcement required to suppress anomalies. Coordination function: maintaining stable institutional narratives. Extraction mechanism: suppressing genuine dissent alongside noise.
constraint_indexing:constraint_classification(confirmation_bias, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: COGNITIVE-RATIONALIST IDEAL (PITON) — The aspiration to pure rationality and unbiased reasoning persists in education, science, and public discourse despite decades of psychological evidence that confirmation bias is fundamental to cognition. The ideal is largely performative — lip service to objectivity masks actualized bias in all institutions. Theater ratio (0.68) reflects that institutions maintain the 'rational deliberation' facade while structurally encoding confirmatory processes. Piton classification: the rational-agent ideology persists through inertia, not empirical validity.
constraint_indexing:constraint_classification(confirmation_bias, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From a civilizational perspective, confirmation bias is an inherent property of information processing: bounded rationality, pattern-matching neural architecture, and finite attention make selective filtering (including confirmatory filtering) inevitable. No agent can access all information or process without prior structure. This perspective sees confirmation bias as a natural law of cognition. However, the distinction between unavoidable selective attention and amplified confirmation through social/algorithmic systems is critical — the first is mountain-like; the second is socially contingent extraction. The engine's analysis reveals that the 'natural law' framing conflates two distinct constraints.
constraint_indexing:constraint_classification(confirmation_bias, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(confirmation_bias_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(confirmation_bias, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(confirmation_bias, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(confirmation_bias, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(confirmation_bias, TR),
    TR >= 0.70.

:- end_tests(confirmation_bias_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The baseline confirmation bias is cognitive architecture (unavoidable), but social and algorithmic amplification creates genuine extraction. Algorithmic amplification increased from ~0.05x multiplier (early social media) to ~2.5x multiplier (current recommendation systems) over the 20-year interval. At current levels, the amplification is not a natural feature but a designed choice. The value (0.58) reflects that amplification is significant but not total — individuals can still encounter contrary views, institutions can still update beliefs, though both require deliberate effort. Suppression (0.62): Moderate-high. Algorithmic filtering creates visibility suppression (minority views downranked), social suppression (conformity pressures), institutional suppression (dissent-management costs), and psychological suppression (identity protection). But suppression is not total — whistleblowers exist, heterodox views circulate, institutions sometimes update. Theater ratio (0.68): High and rising. Institutions maintain the rhetoric of objective reasoning, unbiased deliberation, and fair information access while structurally encoding confirmatory processes. Academic peer review, media editorial standards, and regulatory processes all claim impartiality while demonstrating systematic bias. Theater increased as psychological evidence of bias became mainstream — institutions now must perform objectivity while maintaining confirmatory structures.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why indexical classification is necessary. The same structural phenomenon — biased information processing — appears as rope (beneficiary institution), tangled-rope (mixed individual experience), snare (minority trapped), piton (rationalist theater), and mountain (cognitive inevitability) from different structural positions. The algorithmic beneficiary experiences rope — solving the coordination problem of matching users to relevant content. The minority viewpoint holder experiences snare — suppressed with no exit. The individual believer experiences tangled-rope — genuine psychological benefit but epistemic cost. The rationalist ideal is piton — the rhetoric of objectivity persists through institutional inertia despite evidence that bias is fundamental. The cognitive architect (mountain view) is a false summit — naturalizes socially amplified extraction as inevitable cognition. The perspectival gap reveals that 'confirmation bias' is not one thing: unavoidable cognitive filtering is mountain-like; socially amplified and algorithmically engineered bias is extraction. The constraint story must separate these.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position: who benefits from confirmation bias amplification and who bears costs. Algorithmic systems (institutional/arbitrage) derive d ≈ 0.05 (full beneficiary) — engagement optimization directly rewards bias amplification; they have arbitrary exit capacity (could be modified) but benefit from current design. Institutional consensus enforcers (organized/constrained) derive d ≈ 0.35 (moderate beneficiary) — consensus maintenance reduces dissent costs but constrained by empirical reality and legitimacy requirements; partial beneficiary status. Individual believers (moderate/constrained) derive d ≈ 0.50 (symmetric) — psychological benefit of confirmationist closure balanced against epistemic costs of poor decisions and polarization; both costs and benefits are real. Minority viewpoint holders (powerless/trapped) derive d ≈ 0.92 (near-total target) — face suppression with no exit; trapped by identity and asymmetric costs to belief change. Epistemic commons (powerless/trapped, abstract) derives d ≈ 0.95 — cannot organize, cannot exit, bears information degradation costs with no benefit. The engine computes χ from these d values and the sigmoid f(d), producing the experienced extractiveness for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The risk is that confirmation bias appears to be pure inevitable cognition (mountain) when much of the social damage is from engineered amplification (snare/tangled-rope). This naturalizes social/technical choices as laws of mind. The resolution is empirical: decompose baseline cognitive filtering from algorithmic/social amplification. The cognitive baseline (what isolated humans filter regardless of context) is mountain-like — inevitable given bounded rationality. The amplification layer (algorithmic ranking, network homophily, institutional consensus enforcement) is contingent extraction. The constraint story treats them as coupled phenomena: confirmation bias is the substrate, social amplification is the mechanism, extractive constraint is the outcome. The snare and tangled-rope classifications are justified because the suppression (0.62) and extraction (0.58) exceed what would occur from cognition alone. Without social/algorithmic amplification, the constraint would classify as rope (coordination around shared beliefs) or piton (belief maintenance theater). The amplification transforms it into snare for minorities and tangled-rope for majorities. The mandatrophy is resolved by showing that the institutional consensus-enforcement perspective is genuinely tangled-rope (benefits from stability, constrained by empirical reality) rather than snare, while the minority perspective is genuinely snare (no benefits, maximum suppression, zero exit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bias_amplification_threshold,
    'What degree of algorithmic/social amplification transforms unavoidable cognitive filtering into extractive constraint?',
    'Comparison of confirmation bias effects in isolated individuals vs networked populations; measurement of belief divergence rates with/without algorithmic amplification; isolation of cognitive baseline from technological enhancement',
    'If threshold low (~0.15 amplification factor): natural cognitive bias IS the constraint. If threshold high (>0.50): only socially/technologically amplified bias counts as extraction; natural bias is cognitive architecture (mountain). This determines whether the snare/tangled-rope classifications are real or artifacts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bias_amplification_threshold, empirical, 'Degree of amplification that transforms cognitive bias into extractive constraint').

omega_variable(
    exit_cost_asymmetry,
    'Do minority viewpoint holders genuinely face higher costs to exit their informational constraints than majority holders?',
    'Longitudinal tracking of belief change rates across majority/minority position holders; measurement of social/career costs for changing positions; comparison of effort required for exposure to contrary evidence',
    'If costs are symmetric: confirmation bias is cognitive symmetry (unchosen for both), not extraction. If costs are asymmetric: minority holders are trapped (snare confirmed). This determines whether perspectives 2 and 4 are accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_asymmetry, empirical, 'Whether exit costs differ for majority vs minority viewpoint holders').

omega_variable(
    algorithm_intent_vs_effect,
    'Is social media algorithmic amplification of confirmation bias intentional extraction design or incidental optimization for engagement?',
    'Analysis of platform design documentation and A/B testing; interviews with engineers; comparison of engagement-optimized systems vs bias-mitigated alternatives; measurement of revenue impact from bias amplification vs other engagement levers',
    'If intentional: algorithms are conscious extractors (snare/tangled-rope justified). If incidental: algorithms are externality generators (rope with negative spillover, not Snare). This affects whether technological beneficiaries should be classified as active enforcers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_intent_vs_effect, empirical, 'Whether algorithmic bias amplification is intentional or incidental').

omega_variable(
    institutional_dissent_capacity,
    'Can institutions maintain legitimacy while empirically responding to minority views, or does consensus enforcement require suppression?',
    'Analysis of institutional belief change in response to evidence; comparison of legitimacy metrics for institutions that update publicly vs those that suppress dissent; measurement of institutional survival rates across belief-update strategies',
    'If institutions can update: tangled-rope classification valid (genuine coordination function alongside extraction). If institutions require suppression: more snare-like (extraction necessary for institutional coherence). This determines whether institutional perspectives are hybrids or pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_dissent_capacity, conceptual, 'Whether institutions can maintain legitimacy while updating beliefs empirically').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(confirmation_bias, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(confbias_tr_t0, confirmation_bias, theater_ratio, 0, 0.35).
narrative_ontology:measurement(confbias_tr_t10, confirmation_bias, theater_ratio, 10, 0.55).
narrative_ontology:measurement(confbias_tr_t20, confirmation_bias, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(confbias_be_t0, confirmation_bias, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(confbias_be_t10, confirmation_bias, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(confbias_be_t20, confirmation_bias, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(confirmation_bias, information_standard).
narrative_ontology:affects_constraint(confirmation_bias, algorithmic_filter_bubble).
narrative_ontology:affects_constraint(confirmation_bias, institutional_consensus_enforcement).
narrative_ontology:affects_constraint(confirmation_bias, polarization_in_distributed_networks).

% DUAL FORMULATION NOTE:
% Confirmation bias decomposes into cognitive baseline (mountain-like inevitability) and social/algorithmic amplification (extraction constraint). The constraint family includes: cognitive_filtering_baseline (ε=0.05, mountain), confirmation_bias (ε=0.58, tangled-rope, this story), and algorithmic_amplification (ε=0.45, tangled-rope). Cognitive baseline is upstream; this story represents the socially amplified phenomenon; algorithmic amplification represents the technical instantiation. Link them via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(confirmation_bias, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
