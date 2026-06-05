% ============================================================================
% CONSTRAINT STORY: rational_actor_assumption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rational_actor_assumption, []).

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
 *   constraint_id: rational_actor_assumption
 *   human_readable: Rational Actor Assumption in Economic and Political Analysis
 *   domain: economics/political_science/organizational_theory
 *
 * SUMMARY:
 *   The rational actor assumption (RAA) in economics, political science, and
 *   organizational analysis represents a foundational constraint on how these
 *   disciplines model human behavior, institutional choice, and resource
 *   allocation. The RAA stipulates that actors have well-defined preferences,
 *   access to relevant information, computational capacity to optimize, and
 *   act to maximize utility or payoff. This constraint exhibits simultaneous
 *   coordination function (enabling mathematically tractable models and
 *   comparative analysis) and extraction function (suppressing alternative
 *   frameworks, channeling resources toward RAA-compatible research, and
 *   producing policy failures when real behavior deviates). The constraint's
 *   theater ratio has risen from 0.35 (mid-20th century, when RAA was
 *   empirically competitive with limited alternatives) to 0.70 (contemporary,
 *   where behavioral anomalies are well-documented but RAA persists through
 *   institutional momentum). Extractiveness has increased from 0.48 to 0.62
 *   as alternatives have become more viable but institutional enforcement has
 *   simultaneously strengthened. The constraint is a diagnostic case of
 *   Goodhart drift: the metric (tractability, mathematical elegance,
 *   falsifiability in principle) has decoupled from the target (predictive
 *   accuracy of human behavior).
 *
 * KEY AGENTS:
 *   - Quantitative Modelers: Primary beneficiary (institutional/arbitrage) — RAA provides analytical convenience, publishability, career advancement within mainstream economics
 *   - Behavioral Researchers: Primary victim (powerless/trapped) — anomalies systematically reinterpreted through auxiliary hypotheses; career barriers to paradigm critique
 *   - Policy Practitioners: Secondary victim (moderate/constrained) — implement RAA-based policies that fail in predictable ways; bear reputational cost when bounded reality emerges
 *   - Behavioral Economics Coalition: Organized agents (organized/mobile) — building alternatives with institutional footholds (labs, centers, funding), creating sunset dynamics
 *   - Formal Theory Establishment: Institutional actor (institutional/arbitrage) — maintains RAA through publication standards, department hiring, PhD requirements; increasingly sees own framework as performative
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing mathematical convenience as logical necessity rather than examining its extractive enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rational_actor_assumption, 0.62).
domain_priors:suppression_score(rational_actor_assumption, 0.58).
domain_priors:theater_ratio(rational_actor_assumption, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rational_actor_assumption, extractiveness, 0.62).
narrative_ontology:constraint_metric(rational_actor_assumption, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rational_actor_assumption, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rational_actor_assumption, tangled_rope).
narrative_ontology:human_readable(rational_actor_assumption, "Rational Actor Assumption in Economic and Political Analysis").
narrative_ontology:topic_domain(rational_actor_assumption, "economics/political_science/organizational_theory").

domain_priors:requires_active_enforcement(rational_actor_assumption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rational_actor_assumption, quantitative_modelers).
narrative_ontology:constraint_beneficiary(rational_actor_assumption, institutional_economists).
narrative_ontology:constraint_beneficiary(rational_actor_assumption, policy_analysts).
narrative_ontology:constraint_victim(rational_actor_assumption, behavioral_anomalies).
narrative_ontology:constraint_victim(rational_actor_assumption, bounded_rationality_researchers).
narrative_ontology:constraint_victim(rational_actor_assumption, non_western_epistemic_traditions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BEHAVIORAL RESEARCHER (SNARE) — Trapped in a disciplinary framework where experimental evidence of non-rational behavior is systematically discounted or reinterpreted as 'preference revelation' through auxiliary hypotheses. Cannot exit without abandoning career trajectory. Bears full cost of the rationality assumption's protective belt — every deviation from rational prediction requires explanation within the RAA framework rather than questioning the framework itself.
constraint_indexing:constraint_classification(rational_actor_assumption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: POLICY PRACTITIONER (TANGLED ROPE) — Experiences both coordination benefit and extraction. The RAA enables systematization of policy analysis (benefit: models make recommendations tractable and comparable). But the assumption forces practitioners to ignore observational evidence that contradicts it, leading to policy failures. Exit is costly — abandoning quantitative metrics means reduced authority and funding, but adherence produces suboptimal outcomes. Mixed structure.
constraint_indexing:constraint_classification(rational_actor_assumption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: QUANTITATIVE MODELER (ROPE) — Primary beneficiary. The RAA provides enormous analytical convenience — tractable optimization problems, falsifiable predictions, publishable results. Experiences the assumption as a pure coordination mechanism: it enables collaborative knowledge-building with other modelers worldwide. Can exit at low cost (behavioral economics is increasingly mainstream), but benefits from current RAA dominance are significant. Net beneficiary position.
constraint_indexing:constraint_classification(rational_actor_assumption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: BEHAVIORAL ECONOMICS COALITION (SCAFFOLD) — Organized agents (Kahneman, Tversky's descendants, nudge practitioners) have built alternative frameworks with sunset logic. The RAA persists but is increasingly perceived as a temporary simplification being replaced by bounded rationality models, satisficing, and ecological rationality. The coalition has exit pathways and institutional footholds. High perceived extractiveness but with visible alternatives and sunset trajectory. Theater ratio is lower in behavioral domains where practitioners explicitly acknowledge bounded rationality.
constraint_indexing:constraint_classification(rational_actor_assumption, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: FORMAL EQUILIBRIUM THEORY ESTABLISHMENT (PITON) — The mathematical machinery of general equilibrium, game theory, and rational expectations has become largely performative. Practitioners maintain the RAA in models not because empirical evidence supports it, but because the mathematical infrastructure depends on it. The assumption persists through institutional inertia — journal prestige, dissertation requirements, department hiring standards — despite widespread private acknowledgment of its limitations. Theater ratio is extremely high: RAA-based models are published and cited despite known anomalies, sustained by the ritual rather than by predictive power.
constraint_indexing:constraint_classification(rational_actor_assumption, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MATHEMATICAL NECESSITY VIEW (MOUNTAIN) — From a pure mathematics perspective, some idealization is inherent to formal analysis: continuous utility functions, complete information, perfect recall, and infinite rationality are simplifying assumptions necessary to make tractable models at all. This perspective sees the RAA as an unavoidable feature of mathematical formalization itself — a natural law of formalized social science. However, the structural data (high suppression, high theater, active enforcement, organized alternatives) contradicts the mountain classification — the engine will compute this as a false summit, revealing that mathematical convenience has been naturalized as logical necessity.
constraint_indexing:constraint_classification(rational_actor_assumption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rational_actor_assumption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rational_actor_assumption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rational_actor_assumption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rational_actor_assumption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rational_actor_assumption, TR),
    TR >= 0.70.

:- end_tests(rational_actor_assumption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The RAA extracts from those whose models don't fit rational prediction — behavioral researchers spend careers explaining anomalies, practitioners implement suboptimal policies, non-Western epistemic traditions (relational decision-making, collectivist rationality, sacred economics) are dismissed as 'irrational' rather than investigated as alternative frameworks. The extraction is not complete (behavioral economics exists and grows) but substantial (mainstream journals, funding, and hiring still favor RAA). Suppression (0.58): Moderate-high. Barriers include: career risk of questioning foundational assumptions, publication bias against RAA critiques, PhD curriculum that trains practitioners into RAA thinking, funding concentration in quantitative approaches, and the protective belt phenomenon (Lakatos) where auxiliary hypotheses absorb anomalies. But suppression is not total — behavioral economics, evolutionary game theory, and agent-based modeling all provide escape routes. Theater ratio (0.68): High. Contemporary formal economics maintains RAA models in prestigious journals despite widespread acknowledgment (in private conversations and bounded-rationality sidelines) that humans are not rational optimizers. The ritual persists: models are built, submitted, reviewed, published, and cited according to RAA standards regardless of empirical fit. The journal system itself enforces the theater — models that drop RAA in favor of explicit bounded rationality or ecological rationality are perceived as less 'rigorous' even when they predict better.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full tension between mathematical formalization and empirical accuracy. The quantitative modeler sees the RAA as enabling coordination and reproducible science (Rope) — different researchers worldwide can build on the same formal framework. The behavioral coalition sees a temporary constraint being displaced by bounded rationality models (Scaffold) — they perceive clear sunset as alternatives mature. The policy practitioner experiences extraction directly (Tangled Rope) — the system requires RAA adoption while producing failures. The behavioral researcher bears the brunt (Snare) — their observations are trapped within the RAA interpretive framework with no escape without career destruction. The formal theory establishment sees its own degradation (Piton) — peer-reviewed journals publish RAA-based models that both they and reviewers know are empirically inadequate, sustained by prestige and institutional structure rather than function. The civilizational analytical observer risks seeing mathematical inevitability (Mountain) — formalization requires simplification — but this naturalizes what is a choice: other formalisms (agent-based, network dynamical systems, qualitative comparative analysis) avoid or minimize the RAA without sacrificing rigor.
 *
 * DIRECTIONALITY LOGIC:
 *   Quantitative modelers: beneficiary + arbitrage → d ≈ 0.15 → low χ from this perspective. Behavioral researchers: victim + trapped → d ≈ 0.95 → high χ from this perspective. Policy practitioners: victim + constrained → d ≈ 0.75 → high χ. Behavioral coalition: partially victim + organized + mobile → d ≈ 0.50 → moderate χ, reduced by counter-power. Formal establishment: beneficiary despite piton status + arbitrage → d ≈ 0.08 → negative χ (they benefit despite degradation recognition). Analytical observer at canonical fallback for analytical atoms: d ≈ 0.73.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The RAA constraint resolves its mandate tension by showing that the constraint provides genuine coordination (enabling formal analysis, comparative institutional study, quantitative policy guidance) alongside genuine extraction (suppressing alternatives, protecting RAA practitioners from empirical challenge, channeling resources toward compatible research). The constraint is neither pure coordination (rope) nor pure extraction (snare) — it is tangled rope with rising theater. The mandate — 'should we use rationality assumptions in modeling?' — is genuinely mixed: rationality assumptions enable tractable analysis AND limit empirical discovery. The resolution is not to eliminate RAA (which would collapse analytical capacity) but to consciously reduce its enforcement, expand visibility of alternatives, and permit empirical comparison. The false summit at the analytical context (mathematical inevitability view) is the key diagnostic: it naturalizes a choice as a necessity. Recognizing this choice opens the option space. Theater ratio rising from 0.35 to 0.70 indicates increasing theatrical maintenance — the constraint's coordinating function is stable or declining while enforcement intensifies. This is the signature of impending instability: excessive theater precedes constraint reformation or collapse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_content_of_rationality,
    'What constitutes falsification of the rational actor assumption? Does any observed behavior count as rational through post-hoc preference revelation?',
    'Examine the Lakatos research programme: how many auxiliary hypotheses (incomplete information, revealed preferences, metapreferences, time-inconsistent preferences) can be added before the RAA becomes unfalsifiable? Count the ad-hoc modifications required to reconcile theory with observed anomalies.',
    'If RAA is unfalsifiable: the constraint is not a scientific assumption but a definitional framework (moves toward piton/snare classification). If falsifiable: the empirical status of the RAA can be evaluated directly (permits rope/tangled_rope distinction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(empirical_content_of_rationality, conceptual, 'Whether rational actor assumption is falsifiable or becomes unfalsifiable through protective belt expansion').

omega_variable(
    alternative_model_predictive_power,
    'Do bounded rationality, satisficing, and ecological rationality models systematically outpredict RAA-based models on held-out data across multiple domains?',
    'Meta-analysis of prediction accuracy: compare forecasting errors of RAA models vs behavioral models on financial markets, consumer behavior, political voting, organizational decisions. Standardize across publication bias and researcher degrees of freedom.',
    'If behavioral models consistently outpredict: RAA constraint is extractive mechanism (snare/tangled_rope confirmed). If RAA remains competitive: coordination benefit may justify the assumption (rope may be correct perspective). If mixed: domain-specific classification required (separate stories by domain).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_predictive_power, empirical, 'Relative predictive power of rational actor vs bounded rationality models').

omega_variable(
    disciplinary_enforcement_mechanisms,
    'What enforcement mechanisms maintain RAA dominance despite known empirical limitations? Are editorial filters, PhD curriculum requirements, or funding allocation mechanisms active suppressors of alternatives?',
    'Content analysis of journal rejection rates for papers critiquing RAA vs proposing alternatives; analysis of PhD economics curriculum; funding allocation patterns (NSF, private foundations) across rational vs behavioral approaches; survey of economists'' private beliefs vs published positions.',
    'If enforcement is active and intentional: classification is snare or tangled_rope (extraction confirmed). If enforcement is institutional inertia: piton classification confirmed. If enforcement is weak: rope or scaffold may be correct (suggests rapid displacement timeline).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disciplinary_enforcement_mechanisms, empirical, 'Degree and mechanism of active enforcement of rational actor assumption').

omega_variable(
    identity_lock_in_economics_training,
    'To what extent are economists'' identities constituted through rationality assumption expertise, making exit from RAA-dependent frameworks equivalent to identity dissolution?',
    'Career trajectory analysis of economists who shifted from RAA to behavioral frameworks; implicit association tests linking economist identity with rational choice theory; interviews documenting experience of paradigm shift.',
    'If identity lock is strong: some economist perspectives should be classified as identity_locked rather than simply constrained; this would show rope classification at biographical horizon rather than mountain (revealing cognitive rather than structural immobility). If weak: standard constrained exit options apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_economics_training, empirical, 'Degree of identity fusion between economists and rational actor frameworks').

omega_variable(
    translation_between_frameworks,
    'Can rational actor predictions be systematically translated to behavioral models through appropriate preference and information parameters, making them equivalent formalisms? Or are they fundamentally incompatible frameworks?',
    'Formal analysis of correspondence: test whether every RAA prediction can be expressed as a bounded rationality prediction with suitably chosen constraints. Identify core predictions where translation fails.',
    'If equivalent: frameworks are different formalizations of the same underlying reality (RAA is rope coordinate, not extraction). If incompatible: they make conflicting predictions (one extracts through false legitimation). If partially equivalent: domain-specific equivalence mapping may be required.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(translation_between_frameworks, conceptual, 'Whether rational actor and behavioral models are mathematically equivalent or fundamentally incompatible').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rational_actor_assumption, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(raa_tr_t0, rational_actor_assumption, theater_ratio, 0, 0.35).
narrative_ontology:measurement(raa_tr_t20, rational_actor_assumption, theater_ratio, 20, 0.52).
narrative_ontology:measurement(raa_tr_t40, rational_actor_assumption, theater_ratio, 40, 0.68).
narrative_ontology:measurement(raa_tr_t60, rational_actor_assumption, theater_ratio, 60, 0.7).

% Extraction over time
narrative_ontology:measurement(raa_be_t0, rational_actor_assumption, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(raa_be_t20, rational_actor_assumption, base_extractiveness, 20, 0.56).
narrative_ontology:measurement(raa_be_t40, rational_actor_assumption, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(raa_be_t60, rational_actor_assumption, base_extractiveness, 60, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rational_actor_assumption, information_standard).
narrative_ontology:boltzmann_floor_override(rational_actor_assumption, 0.12).
narrative_ontology:affects_constraint(rational_actor_assumption, expected_utility_theory).
narrative_ontology:affects_constraint(rational_actor_assumption, perfect_information_assumption).
narrative_ontology:affects_constraint(rational_actor_assumption, representative_agent_model).
narrative_ontology:affects_constraint(rational_actor_assumption, efficient_markets_hypothesis).

% DUAL FORMULATION NOTE:
% The RAA is upstream of multiple economic constraints that depend on it. Expected utility theory, perfect information assumption, representative agent models, and EMH all treat rationality as foundational. The RAA itself decomposes into multiple structurally distinct constraints with different ε values: mathematical tractability (ε≈0.05, rope), empirical predictiveness (ε≈0.72, snare), and policy guidance credibility (ε≈0.58, tangled_rope). This story addresses the meta-assumption; downstream stories address specific instantiations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rational_actor_assumption, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
