% ============================================================================
% CONSTRAINT STORY: credulity_learning_lag
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_credulity_learning_lag, []).

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
 *   constraint_id: credulity_learning_lag
 *   human_readable: Credulity Learning Lag: The Asymmetry Between Belief Adoption and Correction
 *   domain: cognitive_epistemology/social_dynamics
 *
 * SUMMARY:
 *   Credulity learning lag — the structural asymmetry between the speed of
 *   belief adoption and the speed of belief correction — creates a generative
 *   extraction mechanism that operates at multiple scales: individual
 *   psychology, institutional incentive structures, algorithmic
 *   amplification, and social prestige allocation. An agent encountering a
 *   plausible false claim must choose between rapid acceptance (using the
 *   claim as a working model despite unverified status) and costly
 *   verification (investing time and expertise to evaluate the claim before
 *   adoption). The rational choice for most agents is rapid acceptance,
 *   leading to population-wide belief in claims that are later identified as
 *   false. The correction, when it arrives, spreads more slowly and reaches
 *   fewer people than the original false claim — a phenomenon robustly
 *   observed across domains (misinformation, medical pseudoscience, political
 *   claims, conspiracy theories). This lag is simultaneously (1) an
 *   adaptation to human cognitive limits (we must operate on incomplete
 *   information), (2) a social coordination problem (how do we efficiently
 *   separate true from false claims in a large epistemic commons), (3) an
 *   institutional extraction mechanism (initial claimants benefit from the
 *   lag; fact-checkers and epistemic authorities benefit from proliferation
 *   of false claims requiring correction), and (4) an algorithmic choice
 *   (social media platforms amplify speed-first signals over accuracy-first
 *   signals). The constraint's claimed type is tangled rope because it
 *   exhibits both genuine coordination function (society must solve the
 *   problem of collective belief formation) and asymmetric extraction
 *   (initial claimants and misinformation propagators capture benefits during
 *   the lag window).
 *
 * KEY AGENTS:
 *   - Epistemically Trapped Public: Primary victim (powerless/trapped) — exposed to false claims, adopts them due to cognitive limits, bears cost of operating with false models; cannot exit informational environment or easily correct belief
 *   - Initial Claimants: Primary beneficiary (institutional/arbitrage) — benefits from lag window; claim reaches maximum spread before correction; can arbitrage between defending claim or pivoting narrative
 *   - Misinformation Propagators: Secondary beneficiary (institutional/arbitrage) — intentionally exploit the lag; distribute claims knowing corrections will arrive later and reach fewer people
 *   - Fact-Checking Community: Moderate actor (moderate/constrained) — coordinate truth-correcting role while capturing authority and resources; funding and prestige increase as misinformation proliferates; constrained by dependence on the misinformation ecosystem
 *   - Epistemic Authorities (universities, journals, institutions): Institutional actor (institutional/arbitrage) — benefit from prestige differential between speed and accuracy; can arbitrage correction authority
 *   - Platform Algorithms: Institutional infrastructure (institutional/arbitrage) — amplify speed-first signals; designed to maximize engagement which correlates with novelty/surprise; create institutional lock-in to speed-first norms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent architectural choices (platform algorithm design, prestige allocation, social signal asymmetry) as inherent cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(credulity_learning_lag, 0.58).
domain_priors:suppression_score(credulity_learning_lag, 0.65).
domain_priors:theater_ratio(credulity_learning_lag, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(credulity_learning_lag, extractiveness, 0.58).
narrative_ontology:constraint_metric(credulity_learning_lag, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(credulity_learning_lag, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(credulity_learning_lag, tangled_rope).
narrative_ontology:human_readable(credulity_learning_lag, "Credulity Learning Lag: The Asymmetry Between Belief Adoption and Correction").
narrative_ontology:topic_domain(credulity_learning_lag, "cognitive_epistemology/social_dynamics").

domain_priors:requires_active_enforcement(credulity_learning_lag).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(credulity_learning_lag, initial_claimants).
narrative_ontology:constraint_beneficiary(credulity_learning_lag, misinformation_propagators).
narrative_ontology:constraint_victim(credulity_learning_lag, epistemic_commons).
narrative_ontology:constraint_victim(credulity_learning_lag, correction_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EPISTEMICALLY TRAPPED PUBLIC (SNARE) — Agents exposed to a false claim and unable to invest sufficient cognitive resources to verify it before accepting it. Once adopted, belief persists due to backfire effects, identity fusion with the claim, and asymmetric exposure to corrections. Maximum extraction because the public bears the full cost of living with false models and cannot easily exit the informational environment.
constraint_indexing:constraint_classification(credulity_learning_lag, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FACT-CHECKING COMMUNITY (TANGLED ROPE) — Coordinating truth-correcting role (needed function) alongside extraction of authority and attention. Fact-checkers benefit from the lag itself — their resources, prestige, and funding increase as false claims proliferate. Career advancement depends on discovering errors to correct rather than preventing errors from spreading. Constrained exit due to reputational and economic dependence on the misinformation ecosystem.
constraint_indexing:constraint_classification(credulity_learning_lag, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INITIAL CLAIMANT (ROPE) — Experiences the lag as pure coordination: the time between claim and correction is when the claim achieves maximum spread, citation impact, and influence. Corrections come later, acknowledged by fewer people, with less social signal. The claimant experiences the constraint as enabling optimal communication of their narrative. Net beneficiary with arbitrage options — can exit to defend the claim, challenge the correction, or pivot narrative.
constraint_indexing:constraint_classification(credulity_learning_lag, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: EPISTEMIC NORM BUILDER (SCAFFOLD) — Organized agents (education systems, media literacy programs, platform correction algorithms, institutional fact-checking standards) see the lag as a temporary problem with a sunset clause. Improved pre-verification norms, distributed epistemic authority, and algorithmic intervention are building alternative belief-adoption pathways that reduce lag duration. Sunset horizon: 15-30 years as digital literacy matures and platform architectures change to prioritize correction visibility.
constraint_indexing:constraint_classification(credulity_learning_lag, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SOCIAL PRESTIGE SYSTEM (PITON) — The social reward structure for being 'first to speak' vs 'correct to speak' heavily weights speed. The prestige lag (rewards accrue to early speakers, corrections arrive later with less social visibility) is largely performative inertia — institutional structures like academic journals, news cycles, and social media algorithms have ossified around speed-first norms. The theater ratio reflects that prestige allocation is ceremonial rather than epistemically functional. Inertia maintains the structure despite degraded epistemic function.
constraint_indexing:constraint_classification(credulity_learning_lag, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational/universal perspective, some credulity lag is inherent to belief formation: humans must adopt models faster than they can verify them to function, and the lag between belief and correction is a structural feature of bounded rationality. This perspective risks naturalizing what is actually a contingent institutional arrangement (prestige allocation, platform architecture, social signal asymmetry) as an immutable property of human cognition.
constraint_indexing:constraint_classification(credulity_learning_lag, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(credulity_learning_lag_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(credulity_learning_lag, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(credulity_learning_lag, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(credulity_learning_lag, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(credulity_learning_lag, TR),
    TR >= 0.70.

:- end_tests(credulity_learning_lag_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The lag creates measurable extraction: initial claimants gain citation advantage, funding access, and narrative control during the 2-5 year verification window; misinformation propagators gain distributed amplification before correction; fact-checkers gain prestige and resources from proliferation of false claims. The extraction is not total (some corrections reach people; some false claims fail to spread) but substantial. The measurement trajectory shows acceleration: extractiveness rises from 0.35 to 0.58 over the interval, indicating that institutional and algorithmic amplification of the lag has increased over time (social media platforms have optimized for speed over accuracy more aggressively in recent years). Suppression (0.65): High. Multiple mechanisms prevent agents from exiting the lag: (1) cognitive limits make rapid belief adoption rational for most people; (2) backfire effects and identity fusion make correction costly; (3) social penalties (appearing uninformed, admitting error) increase correction cost; (4) algorithmic architecture systematically underweights corrections relative to initial claims; (5) attentional scarcity means corrections compete with new claims for limited cognitive resources. Theater ratio (0.68): High. The social rituals around correction — fact-checking ceremonies, journalistic corrections, academic retractions — are substantially performative. Fact-checkers publish corrections that reach a fraction of the original claim's audience. Journals issue retractions that most readers never see. The visible performance of correction (we have systems for this!) masks the structural failure of correction to actually change population-level belief. The trajectory from 0.42 to 0.68 reflects increasing reliance on correction theater as algorithmic amplification of claims has accelerated — more performative correction rituals are deployed, but the lag itself persists.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental disagreement concerns whether the lag is inherent to belief formation (mountain) or contingent on institutional choices (tangled rope/snare). The initial claimant sees coordination (rope) — their information spreads efficiently. The trapped public sees extraction (snare) — they must adopt unverified claims and bear the cost of correction asymmetry. The fact-checking community sees mixed signals: coordination role (correcting false claims) mixed with extraction benefit (funding and prestige tied to misinformation prevalence). The epistemic norm-builder sees a temporary coordination failure with engineering solutions (scaffold) — open verification, algorithmic intervention, and media literacy can reduce the lag. The prestige system sees its own degraded ritual (piton) — the speed-first norm persists through inertia despite producing worse epistemic outcomes. The civilizational observer risks seeing an immutable feature of bounded cognition (mountain) — but the structural data reveals this as naturalization: the lag has accelerated in recent decades not because cognition changed but because platforms algorithmically amplified it.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by structural position in the claim-correction flow. Initial claimants with institutional power and arbitrage options experience d ≈ 0.15 (net beneficiary position during lag window) → low effective extraction (they experience the constraint as enabling their communication). Trapped public agents with no exit options experience d ≈ 0.92 (full target position in the lag) → high effective extraction (they experience maximum experienced cost). Fact-checkers with moderate power and constrained exit (dependent on misinformation ecosystem for funding and prestige) experience d ≈ 0.60 (mixed position: they coordinate corrections but benefit from claim proliferation) → moderate effective extraction. The epistemic commons (abstract victim) has no organized power representation and cannot participate in directionality negotiation — it experiences passive victimization. Algorithmic platforms experience d ≈ 0.08 (net beneficiary: engagement is their optimization target, and the lag maximizes engagement through novelty and dispute) → negative/low effective extraction (platforms experience the constraint as functional coordination of user attention). The credulity lag benefits those positioned to make claims first and those positioned to arbitrage corrections; it extracts from those who must form beliefs in real time and those who bear the social/epistemic cost of living with false models.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC: This constraint resolves the mandatrophy by showing how institutional and technological amplification has transformed a potential feature of bounded rationality into a contingent extraction mechanism. The lag itself (gap between belief and correction) might be unavoidable. But the *magnitude* of the gap — the factor by which initial claims spread more than corrections — is driven by contingent architectural choices: social media algorithms optimize for engagement/novelty, prestige systems reward speed over accuracy, and correction infrastructure is performative rather than functional. The mountain perspective (lag is inherent to cognition) is a false summit: it conflates the irreducible cognitive minimum (some lag exists) with the empirical magnitude we observe (lag has accelerated 10-50x in recent decades as platforms optimized for speed). The correct classification is tangled rope: genuine coordination function (society needs to form collective beliefs efficiently) plus asymmetric extraction (the lag benefits initial claimants and misinformation propagators while harming the epistemic commons and correction-bearers). The mandatrophy surfaces because agents see the same constraint through different structural lenses: from inside the prestige system, the lag is functional (speed is rewarded). From inside algorithmic platforms, the lag is functional (engagement is maximized). From the perspective of the trapped public trying to form accurate models of reality, the lag is purely extractive. The resolution is not 'which perspective is right?' but 'what architectural and institutional changes would reduce the lag without eliminating all benefits of rapid belief adoption?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    backfire_effect_magnitude,
    'Does psychological backfire (corrections strengthen false beliefs) operate at the population level or only for a subset of agents?',
    'Meta-analysis of belief-correction experiments controlling for agent demographics, worldview alignment, and correction framing',
    'If widespread: suppression is structural and near-irreducible (snare dominant). If rare: suppression is context-dependent and reducible through better correction design (tangled rope or scaffold dominant).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backfire_effect_magnitude, empirical, 'Whether psychological backfire operates at population scale').

omega_variable(
    algorithmic_amplification_necessity,
    'Is the credulity lag primarily driven by human cognitive limits (universal/immutable) or by algorithmic amplification of speed-first signals (contingent/changeable)?',
    'Comparison of belief adoption rates and correction lag in pre-social-media environments vs algorithmic-feed environments controlling for claim complexity and agent education',
    'If cognitive: mountain classification appropriate — lag is inherent to human belief formation. If algorithmic: mountain is false summit — lag is extractive institutional choice, tangled rope or snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_amplification_necessity, empirical, 'Whether lag is driven by cognitive limits or algorithmic amplification').

omega_variable(
    identity_lock_vs_cost_barrier,
    'When agents refuse corrections, is the binding mechanism identity fusion (belief fused with self-concept) or material cost (admitting error carries social/economic penalty)?',
    'Intervention studies: isolate identity-affirmation correction frames from cost-reduction frames; measure persistence of false belief when identity cost is removed vs when social cost is removed',
    'If identity fusion dominates: need identity_locked exit classification for affected agents; credulity lag becomes partly interpersonal/relational constraint (attach to relationship/community stories). If cost dominates: constrained exit classification sufficient; focus interventions on changing penalty structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_cost_barrier, empirical, 'Whether belief persistence is driven by identity fusion or material cost').

omega_variable(
    correction_asymmetry_source,
    'Does the asymmetry between claim adoption and correction correction stem from attentional scarcity (corrections compete for limited attention) or from truth asymmetry (false claims are inherently more surprising/shareable)?',
    'Natural experiments: measure spread rates for equally-surprising true vs false claims; control for verification status of claim content; isolate attention via algorithmic intervention',
    'If attentional: architecture interventions (platform design, algorithm modification) can reduce lag. If truth asymmetry: lag is harder to address without changing claim generation incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(correction_asymmetry_source, empirical, 'Whether asymmetry stems from attention scarcity or truth asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(credulity_learning_lag, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cred_tr_t0, credulity_learning_lag, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cred_tr_t5, credulity_learning_lag, theater_ratio, 5, 0.55).
narrative_ontology:measurement(cred_tr_t10, credulity_learning_lag, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(cred_be_t0, credulity_learning_lag, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cred_be_t5, credulity_learning_lag, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(cred_be_t10, credulity_learning_lag, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(credulity_learning_lag, information_standard).
narrative_ontology:boltzmann_floor_override(credulity_learning_lag, 0.12).
narrative_ontology:affects_constraint(credulity_learning_lag, backfire_effect_persistence).
narrative_ontology:affects_constraint(credulity_learning_lag, prestige_asymmetry_allocation).
narrative_ontology:affects_constraint(credulity_learning_lag, algorithmic_amplification_bias).

% DUAL FORMULATION NOTE:
% Credulity learning lag decomposes into at least three structurally distinct constraints: (1) cognitive-limit lag (bounded rationality forces rapid belief adoption) ≈ low ε, mountain/rope, immutable; (2) correction-asymmetry lag (corrections spread slower than claims) ≈ medium ε, tangled rope, partially addressable; (3) prestige-allocation lag (speed-first rewards motivate early claims over accurate claims) ≈ high ε, snare/tangled rope, contingent on institutional choice. Each has different ε, different dominant mechanisms, and different intervention levers. Stories linked via network.affects_constraints to show family structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(credulity_learning_lag, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
