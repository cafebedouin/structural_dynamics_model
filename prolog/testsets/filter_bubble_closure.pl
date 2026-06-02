% ============================================================================
% CONSTRAINT STORY: filter_bubble_closure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_filter_bubble_closure, []).

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
 *   constraint_id: filter_bubble_closure
 *   human_readable: Filter Bubble Closure in Algorithmic Content Systems
 *   domain: media/information_systems/political_economy
 *
 * SUMMARY:
 *   Filter bubble closure in algorithmic content systems creates a
 *   structurally hybrid constraint that solves a genuine coordination problem
 *   (routing infinite content to finite attention) while simultaneously
 *   extracting attention value, suppressing marginal creators, and
 *   degradating the epistemic commons. The constraint operates through
 *   algorithmic invisibility — users are not told what content is suppressed,
 *   why it is suppressed, or what alternatives exist. Marginalized creators
 *   face systematic visibility suppression with no appeal mechanism. The
 *   epistemic commons loses viewpoint diversity through mechanisms that
 *   appear neutral and technical. Platform operators experience the system as
 *   pure coordination (engagement optimization), beneficiary advertisers
 *   experience it as audience routing efficiency, and trapped users/creators
 *   experience it as snare. The core tension is between two real coordination
 *   functions (attention routing and engagement optimization) and real
 *   extraction (value concentration toward high-engagement content and
 *   high-paying advertisers). Unlike pure snares, the filter bubble cannot be
 *   eliminated without degradating user experience; unlike pure ropes, it
 *   cannot be justified purely on coordination grounds without naturalizing
 *   the suppression. This is tangled rope with theater ratio rising over time
 *   — the coordination framing increasingly disguises the extraction
 *   mechanism as platforms refine algorithmic suppression and develop
 *   sophisticated moderation justifications.
 *
 * KEY AGENTS:
 *   - Platform Operator (Institutional/Arbitrage): Primary beneficiary — controls algorithmic routing and captures attention value; experiences system as coordination success
 *   - Marginalized Creator (Powerless/Trapped): Primary victim — faces systematic algorithmic suppression with no exit or appeal mechanism; experiences as snare
 *   - Epistemic Commons (Powerless/Trapped): Structural victim — information diversity is systematically suppressed; abstract collective good with no agency or exit
 *   - Mainstream Creator (Moderate/Constrained): Secondary victim — benefits from amplification in established topics while constrained by algorithmic topic lock-in; experiences tangled rope
 *   - Engagement-Optimized Advertiser (Institutional/Arbitrage): Beneficiary — receives concentrated attention routing to demographic segments; experiences as pure coordination and routing efficiency
 *   - Content Moderation Authority (Institutional/Constrained): Institutional actor — frames algorithmic suppression as protective moderation; maintains theater through dual framing
 *   - User (Moderate/Identity_Locked): Constrained agent whose preferences may be shaped by algorithm exposure; experiences reduced information exposure without recognizing suppression
 *   - Analytical Observer (Analytical/Analytical): Sees both coordination function (real) and extraction mechanism (real); confirms tangled rope classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(filter_bubble_closure, 0.58).
domain_priors:suppression_score(filter_bubble_closure, 0.68).
domain_priors:theater_ratio(filter_bubble_closure, 0.54).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(filter_bubble_closure, extractiveness, 0.58).
narrative_ontology:constraint_metric(filter_bubble_closure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(filter_bubble_closure, theater_ratio, 0.54).

% --- Constraint claim ---
narrative_ontology:constraint_claim(filter_bubble_closure, tangled_rope).
narrative_ontology:human_readable(filter_bubble_closure, "Filter Bubble Closure in Algorithmic Content Systems").
narrative_ontology:topic_domain(filter_bubble_closure, "media/information_systems/political_economy").

domain_priors:requires_active_enforcement(filter_bubble_closure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(filter_bubble_closure, platform_operator).
narrative_ontology:constraint_beneficiary(filter_bubble_closure, high_value_creators).
narrative_ontology:constraint_beneficiary(filter_bubble_closure, engagement_optimized_advertisers).
narrative_ontology:constraint_victim(filter_bubble_closure, marginal_creators).
narrative_ontology:constraint_victim(filter_bubble_closure, epistemic_commons).
narrative_ontology:constraint_victim(filter_bubble_closure, preference_diversity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINALIZED CREATOR (SNARE) — Trapped by algorithmic suppression with no exit capacity. Visibility is gatekept by engagement metrics they cannot influence. Maximum experienced extraction — the algorithm suppresses their content systematically; they bear the cost of closure while benefiting populations capture the gains.
constraint_indexing:constraint_classification(filter_bubble_closure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: EPISTEMIC COMMONS (SNARE) — Structural victim with no agency. Filter bubble closure degradates information diversity; the commons cannot exit or organize. Disconfirming evidence, marginal viewpoints, and minority perspectives are systematically suppressed. This is pure extraction from a commons good with no coordination benefit to the commons itself.
constraint_indexing:constraint_classification(filter_bubble_closure, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences the filter bubble as coordination: routing content to receptive audiences reduces exploration costs, increases engagement, stabilizes user attention patterns. The platform benefits from algorithmic efficiency without perceiving acute extraction — their optimization function aligns with user retention. High arbitrage capacity (can adjust algorithms, modify incentives, or exit market); extraction runs inward from users.
constraint_indexing:constraint_classification(filter_bubble_closure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MAINSTREAM CREATOR (TANGLED ROPE) — Benefits from algorithmic amplification in their domain (established topic authority generates high engagement) while constrained by topic lock-in. The algorithm coordinates audience matching but also extracts by narrowing their viable content range. Exit is costly (migration to other platforms, audience loss) but possible at significant price. Mixed experience: real coordination benefit plus enforcement-backed confinement to profitable niches.
constraint_indexing:constraint_classification(filter_bubble_closure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ENGAGEMENT-OPTIMIZED ADVERTISER (ROPE) — Beneficiary. The filter bubble concentrates attention into predictable demographic segments with high engagement correlation. Algorithmic routing solves their coordination problem: reaching receptive audiences at scale. Low extraction experienced — they are willing participants in the attention capture mechanism. High arbitrage (can adjust bids, switch platforms, segment audiences).
constraint_indexing:constraint_classification(filter_bubble_closure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONTENT MODERATION AUTHORITY (PITON) — Algorithmic content moderation and filter bubble closure appear as distinct functions but operate through identical mechanisms (suppressing visibility). Moderation purports to address harm; the filter bubble purports to optimize engagement. But both work via algorithmic invisibility. Theater ratio is high because moderation justifies itself as protective while the filter bubble operates silently. As a moderation tool, the filter bubble is largely performative — it addresses engagement-as-harm rather than direct content removal. Piton classification reflects this theatrical maintenance of institutional legitimacy.
constraint_indexing:constraint_classification(filter_bubble_closure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational/global perspective, algorithmic content routing solves a genuine coordination problem: infinite information exceeds finite human attention. The filter bubble is the engineered solution to bounded rationality. However, the solution is structurally asymmetric — it coordinates attention flows while extracting value from marginalized sources and the epistemic commons. The analytical view sees both the coordination function (real) and the extraction mechanism (real), making this tangled rope not false summit.
constraint_indexing:constraint_classification(filter_bubble_closure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(filter_bubble_closure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(filter_bubble_closure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(filter_bubble_closure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(filter_bubble_closure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(filter_bubble_closure, TR),
    TR >= 0.70.

:- end_tests(filter_bubble_closure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The filter bubble extracts attention value and advertiser surplus by concentrating visibility toward high-engagement content, but it is not purely extractive because the coordination function is genuine — users do benefit from reduced information exploration costs and content matching. The value reflects that extraction is substantial but mixed with functional coordination. The rise from 0.35 to 0.58 over the interval reflects algorithmic refinement: platforms have tuned engagement optimization more aggressively, increasing extraction while maintaining coordination framing. Suppression (0.68): High. Multiple suppression mechanisms operate simultaneously: (1) algorithmic invisibility of suppressed content (users don't see what they're not shown), (2) lack of appeal mechanisms (suppressed creators have no recourse), (3) engagement metrics as enforcement (marginal topics receive algorithmic punishment), (4) discovery barriers (finding non-recommended content requires active effort). But suppression is not total — users can still access chronological feeds (though hidden), creators can appeal moderation decisions (though rarely successfully), and alternative platforms exist (though friction is high). Theater ratio (0.54): Moderate-high and rising. The constraint is increasingly theatrical because platforms use moderation framing to justify algorithmic suppression. Engagement optimization appears as platform safety; algorithmic invisibility appears as user-friendly curation; marginalization of minority viewpoints appears as quality control. The rise from 0.38 to 0.54 reflects increasing sophistication in institutional justification — transparency reports now include diversity metrics (performing commitment to viewpoint diversity) while algorithmic tuning continues suppressing minority voices. The theater increase reflects growing gap between stated values (platform neutrality, diverse voices, creator opportunity) and operational mechanisms (engagement optimization, algorithmic suppression, attention concentration).
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows maximum perspectival divergence. The platform operator sees a coordination success: the algorithm routes content efficiently, users stay engaged, creators find audiences. The marginalized creator sees a snare: their content is systematically invisible despite quality or effort; the algorithm offers no appeals and no path to visibility. The mainstream creator sees tangled rope: amplification in established domains plus confinement to profitable niches. The epistemic commons sees pure extraction: viewpoint diversity is suppressed with no compensation. The advertiser sees coordination and value. The moderation authority maintains institutional legitimacy through theater — framing algorithmic suppression as protective safety measures. The analytical observer sees both the real coordination function (users do benefit from attention routing) and the real extraction mechanism (value concentration toward high-engagement, marginal voices suppressed, epistemic commons degraded). This perspectival structure is diagnostic of tangled rope: genuine coordination coexists with genuine extraction, and the beneficiaries' experience of pure coordination is predicated on not seeing (or on naturalizing) the victims' experience of snare. The suppression mechanism enforces this perspectival gap: algorithmic invisibility ensures that beneficiaries rarely encounter evidence of suppression, while victims experience it constantly.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from the agent's structural relationship to the extraction flow. Platform operators as beneficiaries with arbitrage exit options derive d ≈ 0.08 (low extraction experienced), applying sigmoid f(d) ≈ -0.12 (negative effective extraction — they benefit). Marginalized creators as victims with trapped exit derive d ≈ 0.95 (maximum extraction experienced), applying sigmoid f(d) ≈ 1.42 (maximum effective extraction). The epistemic commons as powerless victim with no exit derives d ≈ 1.00 (full target), applying sigmoid f(d) ≈ 1.42. Mainstream creators as moderate victims with constrained exit derive d ≈ 0.70 (significant extraction experienced), applying sigmoid f(d) ≈ 1.02. Advertisers as beneficiaries with arbitrage derive d ≈ 0.10 (low extraction), f(d) ≈ -0.08. The scope modifier σ(S) applies globally (σ = 1.2), amplifying effective extractiveness by 20% due to the constraint's planetary scale. Chi values vary by perspective: beneficiaries see χ ≈ 0.35 × (-0.12) × 1.2 ≈ -0.05 (coordination appears as utility), victims see χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (near-maximum effective extraction at global scope). The gap between perspectives is substantial and reveals the structural asymmetry: beneficiaries perceive rope, victims perceive snare, analytical observer sees both functions and perceives tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that filter bubble closure is genuine tangled rope, not false dichotomy. The coordination function is real: routing infinite content to finite attention is a genuine problem, and algorithmic solutions do reduce exploration costs for users and creators. The extraction function is equally real: platforms capture attention surplus, marginalize creators outside high-engagement profiles, and suppress viewpoint diversity. The mandatrophy arises when observers try to classify the filter bubble as pure coordination (rope) or pure extraction (snare). Pure coordination framing naturalizes suppression as efficient routing; pure extraction framing ignores that users do benefit from matched content. The tangled rope classification captures both: the system solves a coordination problem AND it is structurally organized to extract value from that problem's solution. The theater rising over time (0.38 → 0.54) reflects the mandatrophy: as the extraction mechanism becomes more sophisticated, institutional framing becomes more elaborate. Platforms develop 'creator support' programs while refining algorithmic suppression; they publish 'transparency reports' measuring diversity while engagement optimization suppresses diverse voices; they adopt 'content moderation' language while operating purely on algorithmic invisibility. The theater is not dishonest — platforms genuinely do solve coordination problems — but the coordination framing increasingly obscures the extraction mechanism. Resolving the mandatrophy requires keeping both functions visible: the system is coordinative AND extractive, and the rising theater ratio reflects growing institutional effort to present the extractive mechanism as coordinative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_intent_ambiguity,
    'Is the filter bubble closure primarily a technical consequence of engagement optimization or an intentional mechanism to suppress unwanted viewpoints?',
    'Analysis of platform design decisions: A/B testing evidence on algorithmic tuning; internal documentation of engagement metrics vs. diversity metrics; testimony from platform engineers on objective functions; comparison of algorithmic behavior pre/post policy changes',
    'If technical consequence: coordination-focused Rope interpretation gains strength; extraction is incidental byproduct. If intentional design: snare/tangled_rope classification strengthens; extraction is primary function. Changes effective extractiveness f(d) across all perspectives by 0.15-0.30.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_intent_ambiguity, empirical, 'Whether filter bubble is technical artifact or intentional extraction mechanism').

omega_variable(
    marginality_causation_ambiguity,
    'Does algorithmic suppression create marginality (the algorithm makes creators marginal) or does it reflect pre-existing marginality (the algorithm amplifies existing popularity disparities)?',
    'Longitudinal analysis: creators'' visibility and engagement before/after platform algorithm changes; cross-platform comparison (same creator on multiple platforms with different algorithms); network analysis of content flow patterns under different engagement functions',
    'If creates marginality: extraction mechanism is primary (snare/tangled_rope strengthened). If reflects marginality: coordination mechanism dominates (rope strengthened); the constraint may be low-cost information routing rather than high-cost suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(marginality_causation_ambiguity, empirical, 'Whether algorithm creates or reflects creator marginality').

omega_variable(
    exit_barrier_measurement,
    'What is the actual cost structure for users to exit algorithmic filter bubbles (consume disconfirming content, follow non-recommended accounts, use non-algorithmic feeds)?',
    'User behavior studies: friction measurement for accessing algorithmic feeds vs. chronological feeds; success rates for users attempting to diversify content consumption; alternative platform adoption patterns; effort quantification for bubble escape',
    'If exit costs are low: exit_options for users should be ''constrained'' not ''trapped''; classification shifts to tangled_rope from multiple perspectives. If high: ''trapped'' classification confirmed; snare interpretation strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_barrier_measurement, empirical, 'Cost structure for users to exit algorithmic filter bubbles').

omega_variable(
    diversity_metric_existence,
    'Do platforms internally measure and optimize for viewpoint diversity, or is diversity optimization absent from the objective functions platforms use?',
    'Analysis of platform transparency reports and internal documentation; interviews with platform ML teams on diversity constraints in ranking functions; comparison of diversity metrics against engagement metrics in system design; testing for diversity-aware ranking alternatives',
    'If diversity metrics exist and are weighted: coordination function is more robust (rope interpretation gains strength). If absent: extraction is unimpeded (snare/tangled_rope strengthened). Directly affects suppression value 0.10-0.20.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diversity_metric_existence, empirical, 'Whether platforms optimize for viewpoint diversity or engagement-only').

omega_variable(
    creator_preference_autonomy,
    'Do creators actively prefer algorithmic routing of their content to receptive audiences, or is this preference itself shaped by the filter bubble''s enforcement mechanism?',
    'Qualitative research: creator interviews about content strategy preferences pre/post algorithm exposure; comparison of creator choices on platforms with algorithmic vs. chronological feeds; analysis of creator stated preferences vs. revealed preferences (topic choices under algorithm exposure)',
    'If autonomously preferred: coordination function is genuine; rope classification robust. If shaped by enforcement: apparent preference is artifact of suppression; extraction is disguised as coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(creator_preference_autonomy, conceptual, 'Whether creator routing preferences are autonomous or enforcement-shaped').

omega_variable(
    commons_recovery_timescale,
    'If algorithmic filter bubbles were removed, how long would the epistemic commons require to recover diversity of viewpoints and creator visibility distribution?',
    'Counterfactual analysis of platforms that removed algorithmic routing (timeline of Reddit''s r/all, early Twitter chronological feed); simulation of algorithmic removal scenarios; measurement of viewpoint diversity recovery rates; comparison of creator distribution metrics post-policy change',
    'If recovery < 1 year: commons damage is reversible; constraint is tangled_rope with recovery pathway. If recovery > 10 years: commons damage is severe and persistent; snare classification strengthened for epistemic commons perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commons_recovery_timescale, empirical, 'Timescale for epistemic commons recovery after algorithmic removal').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(filter_bubble_closure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(filter_theater_t0, filter_bubble_closure, theater_ratio, 0, 0.38).
narrative_ontology:measurement(filter_theater_t5, filter_bubble_closure, theater_ratio, 5, 0.46).
narrative_ontology:measurement(filter_theater_t10, filter_bubble_closure, theater_ratio, 10, 0.54).

% Extraction over time
narrative_ontology:measurement(filter_extract_t0, filter_bubble_closure, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(filter_extract_t5, filter_bubble_closure, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(filter_extract_t10, filter_bubble_closure, base_extractiveness, 10, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(filter_suppress_t0, filter_bubble_closure, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(filter_suppress_t5, filter_bubble_closure, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(filter_suppress_t10, filter_bubble_closure, suppression_requirement, 10, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(filter_bubble_closure, resource_allocation).
narrative_ontology:affects_constraint(filter_bubble_closure, attention_economy_concentration).
narrative_ontology:affects_constraint(filter_bubble_closure, algorithmic_opacity_enforcement).
narrative_ontology:affects_constraint(filter_bubble_closure, creator_category_lock_in).
narrative_ontology:affects_constraint(filter_bubble_closure, information_asymmetry_amplification).

% DUAL FORMULATION NOTE:
% Filter bubble closure decomposes into three distinct constraint stories: (1) algorithmic engagement optimization (ε ≈ 0.45, tangled rope — genuine coordination problem with extraction overlay), (2) algorithmic opacity/invisibility (ε ≈ 0.72, snare — suppression mechanism), (3) platform-operator control of discovery mechanisms (ε ≈ 0.55, tangled rope — creator access constraint). Each has different ε values and different beneficiary/victim structures. This story focuses on the integrated closure mechanism; see sister stories for component mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(filter_bubble_closure, powerless, 0.98).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
